{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Empire.Server.Graph where

import           Control.Monad.Error                   (throwError)
import           Control.Monad.State                   (StateT)
import qualified Data.Binary                           as Bin
import           Data.ByteString                       (ByteString)
import           Data.ByteString.Lazy                  (fromStrict)
import qualified Data.IntMap                           as IntMap
import           Data.List                             (break, find, partition)
import           Data.List.Split                       (splitOneOf)
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromMaybe, isJust, isNothing)
import qualified Data.Set                              as Set
import           Data.Text                             (stripPrefix)
import qualified Data.Text                             as Text
import           Data.Traversable                      (forM)
import           Data.UUID.Types                       (UUID)
import qualified Data.UUID.Types                       as UUID
import qualified Data.UUID.V4                          as UUID
import           Prologue                              hiding (Item)

import           Empire.API.Data.Breadcrumb            (Breadcrumb (..))
import           Empire.API.Data.Connection            as Connection
import           Empire.API.Data.DefaultValue          (Value (..))
import           Empire.API.Data.Graph                 (Graph (..))
import           Empire.API.Data.GraphLocation         (GraphLocation)
import           Empire.API.Data.Node                  (Node (..), NodeId)
import qualified Empire.API.Data.Node                  as Node
import           Empire.API.Data.NodeMeta              (NodeMeta)
import qualified Empire.API.Data.NodeSearcher          as NS
import           Empire.API.Data.Port                  (InPort (..), OutPort (..), Port (..), PortId (..), PortState (..))
import           Empire.API.Data.PortRef               (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.PortRef               as PortRef
import           Empire.API.Data.TypeRep               (TypeRep (TStar))
import qualified Empire.API.Graph.AddNode              as AddNode
import qualified Empire.API.Graph.AddPort              as AddPort
import qualified Empire.API.Graph.AddSubgraph          as AddSubgraph
import qualified Empire.API.Graph.Code                 as Code
import qualified Empire.API.Graph.Connect              as Connect
import qualified Empire.API.Graph.DumpGraphViz         as DumpGraphViz
import qualified Empire.API.Graph.GetProgram           as GetProgram
import qualified Empire.API.Graph.MovePort             as MovePort
import qualified Empire.API.Graph.NodeResultUpdate     as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearch           as NodeSearch
import qualified Empire.API.Graph.NodesUpdate          as NodesUpdate
import qualified Empire.API.Graph.RemoveConnection     as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes          as RemoveNodes
import qualified Empire.API.Graph.RemovePort           as RemovePort
import qualified Empire.API.Graph.RenameNode           as RenameNode
import qualified Empire.API.Graph.RenamePort           as RenamePort
import qualified Empire.API.Graph.Request              as G
import qualified Empire.API.Graph.SetCode              as SetCode
import qualified Empire.API.Graph.SetDefaultValue      as SetDefaultValue
import qualified Empire.API.Graph.TypeCheck            as TypeCheck
import qualified Empire.API.Graph.UpdateNodeExpression as UpdateNodeExpression
import qualified Empire.API.Graph.UpdateNodeMeta       as UpdateNodeMeta
import           Empire.API.Request                    (Request (..))
import qualified Empire.API.Response                   as Response
import qualified Empire.API.Topic                      as Topic
import           Empire.ASTOp                          (runASTOp)
import qualified Empire.ASTOps.Print                   as Print
import qualified Empire.Commands.Graph                 as Graph
import           Empire.Commands.GraphBuilder          (buildConnections, buildGraph, buildNodes, getNodeName)
import qualified Empire.Commands.GraphUtils            as GraphUtils
import qualified Empire.Commands.Persistence           as Persistence
import           Empire.Empire                         (Empire)
import qualified Empire.Empire                         as Empire
import           Empire.Env                            (Env)
import qualified Empire.Env                            as Env
import           Empire.Server.Server                  (errorMessage, replyFail, replyOk, replyResult, sendToBus')
import           Empire.Utils.TextResult               (nodeValueToText)
import qualified System.Log.MLogger                    as Logger
import           ZMQ.Bus.Trans                         (BusT (..))


logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

notifyCodeUpdate :: GraphLocation -> StateT Env BusT ()
notifyCodeUpdate location = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (resultCode, _) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.getCode location
    case resultCode of
        Left err -> logger Logger.error $ errorMessage <> err
        Right code -> sendToBus' $ Code.Update location $ Text.pack code

notifyNodeResultUpdate :: GraphLocation -> NodeId -> [Value] -> Text -> StateT Env BusT ()
notifyNodeResultUpdate location nodeId values name = sendToBus' $ NodeResultUpdate.Update location nodeId (NodeResultUpdate.Value name values) 42
-- FIXME: report correct execution time

saveCurrentProject :: GraphLocation -> StateT Env BusT ()
saveCurrentProject loc = do
  currentEmpireEnv <- use Env.empireEnv
  empireNotifEnv   <- use Env.empireNotif
  projectRoot      <- use Env.projectRoot
  void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Persistence.saveLocation projectRoot loc

data Expr = Expression        Text
          | Function   (Maybe Text)
          | Module     (Maybe Text)
          | Input      (Maybe Text)
          | Output     (Maybe Text)

parseExpr :: Text -> Expr
parseExpr (stripPrefix "module " -> Just name) = Module   $ Just name
parseExpr (stripPrefix "in "     -> Just name) = Input    $ Just name
parseExpr (stripPrefix "out "    -> Just name) = Output   $ Just name
parseExpr "module"                             = Module     Nothing
parseExpr "in"                                 = Input      Nothing
parseExpr "out"                                = Output     Nothing
parseExpr expr                                 = Expression expr

forceTC :: GraphLocation -> StateT Env BusT ()
forceTC location = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.typecheck location

modifyGraph :: forall req inv res d. (G.GraphRequest req, Response.ResponseResult req inv res ) => (req -> Empire inv) -> (req -> Empire res) -> (Request req -> inv -> res -> StateT Env BusT ()) -> Request req -> StateT Env BusT ()
modifyGraph inverse action success req@(Request uuid guiID request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (inv', _)        <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ inverse request
    case inv' of
        Left err  -> replyFail logger err req (Response.Error err)
        Right inv -> do
            let invStatus = Response.Ok inv
            (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ action request
            case result of
                Left  err    -> replyFail logger err req invStatus
                Right result -> do
                    Env.empireEnv .= newEmpireEnv
                    success req inv result
                    notifyCodeUpdate $ request ^. G.location
                    saveCurrentProject $ request ^. G.location

modifyGraphOk :: forall req inv res d. (Bin.Binary req, G.GraphRequest req, Response.ResponseResult req inv res, Response.ResponseResult req inv ()) => (req -> Empire inv) -> (req -> Empire res) -> (req -> inv -> res -> StateT Env BusT ()) -> Request req -> StateT Env BusT ()
modifyGraphOk inverse action success = modifyGraph inverse action (\req@(Request uuid guiID request) inv res -> replyOk req inv >> success request inv res)

-- helpers

defInverse :: a -> Empire ()
defInverse = const $ return ()

generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

addExpressionNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Maybe NodeId -> Empire Node
addExpressionNode location nodeId expression nodeMeta connectTo = do
    case parseExpr expression of
        Expression expression -> do
            Graph.addNodeCondTC (isNothing connectTo) location nodeId expression nodeMeta
        Function (Just name) -> do
            Graph.addNodeCondTC False location nodeId (Text.append "def " name) nodeMeta
        Module   name -> throwError "Module Nodes not yet supported"
        Input    name -> throwError "Input Nodes not yet supported"
        Output   name -> throwError "Output Nodes not yet supported"

connectNodes :: UUID -> Maybe UUID -> GraphLocation -> Text -> NodeId -> NodeId -> StateT Env BusT ()
connectNodes reqId guiId location expr dstNodeId srcNodeId = do
    let exprCall = head $ splitOneOf " ." $ Text.unpack expr
        inPort   = if exprCall `elem` stdlibFunctions then Arg 0 else Self
        request  = Request reqId guiId $ Connect.Request location (Left $ OutPortRef srcNodeId All) (Left $ InPortRef dstNodeId inPort)
        action (Connect.Request location (Left src) (Left dst)) = Graph.connectCondTC False location src dst
        success _ _ connection                                  = sendToBus' $ Connect.Update location connection
    modifyGraph defInverse action success request
    forceTC location --TODO[MM]: is this not the same as: `Graph.connectCondTC True location src dst` in action???

-- Handlers

mtuple :: (Monad m, Applicative m)=>(a -> m b) -> a -> m ((), b)
mtuple f a = f a >>= \b -> pure ((),b)

handleAddNode :: Request AddNode.Request -> StateT Env BusT ()
handleAddNode = modifyGraph defInverse action success where
    action (AddNode.Request location nodeId expression nodeMeta connectTo) = addExpressionNode location nodeId expression nodeMeta connectTo
    success req@(Request reqId guiId (AddNode.Request location nodeId expression _ connectTo)) inv node = do
        replyResult req inv node
        withJust connectTo $ connectNodes reqId guiId location expression nodeId

handleAddPort :: Request AddPort.Request -> StateT Env BusT ()
handleAddPort = modifyGraph defInverse action replyResult where
    action  (AddPort.Request location portRef) = Graph.addPort location $ portRef ^. PortRef.nodeId --FIXME we should pass whole portRef here

handleAddSubgraph :: Request AddSubgraph.Request -> StateT Env BusT ()
handleAddSubgraph = modifyGraph defInverse action replyResult where
    action (AddSubgraph.Request location nodes connections) = Graph.addSubgraph location nodes connections

handleConnect :: Request Connect.Request -> StateT Env BusT ()
handleConnect = modifyGraph defInverse action replyResult where
    getSrcPort src = case src of
        Left portRef -> portRef
        Right nodeId -> OutPortRef nodeId All
    getDstPort dst = case dst of
        Left portRef -> portRef
        Right nodeId -> InPortRef nodeId Self
    action  (Connect.Request location src dst)                              = Graph.connectCondTC True location (getSrcPort src) (getDstPort dst)

handleRemoveConnection :: Request RemoveConnection.Request -> StateT Env BusT ()
handleRemoveConnection = modifyGraphOk inverse action success where
    inverse (RemoveConnection.Request location dst) = do
        connections <- Graph.withGraph location $ runASTOp buildConnections
        case find (\conn -> snd conn == dst) connections of
            Nothing       -> $notImplemented --TODO[LJK, MM]: Return error here like: Response.Error $ "Cannot find connection by this id: " <> show dst
            Just (src, _) -> return $ RemoveConnection.Inverse src
    action  (RemoveConnection.Request location dst) = Graph.disconnect location dst
    success _ _ _                                   = return ()






handleRemoveNodes :: Request RemoveNodes.Request -> StateT Env BusT ()
handleRemoveNodes = modifyGraphOk inverse action success where
    inverse (RemoveNodes.Request location nodeIds)     = do
        Graph allNodes allConnections monads <- Graph.withGraph location $ runASTOp buildGraph
        let idSet = Set.fromList nodeIds
            nodes = flip filter allNodes       $ \node ->   Set.member (node ^. Node.nodeId)            idSet
            conns = flip filter allConnections $ \conn -> ( Set.member (conn ^. _1 . PortRef.srcNodeId) idSet
                                                         || Set.member (conn ^. _2 . PortRef.dstNodeId) idSet )
        return $ RemoveNodes.Inverse nodes conns
    action  (RemoveNodes.Request location nodeIds)     = Graph.removeNodes location nodeIds
    success (RemoveNodes.Request location nodeIds) _ _ = sendToBus' $ RemoveNodes.Update location nodeIds

handleUpdateNodeExpression :: Request UpdateNodeExpression.Request -> StateT Env BusT ()-- fixme [SB] returns Result with no new informations and change node expression has addNode+removeNodes
handleUpdateNodeExpression = modifyGraph inverse action success where
    inverse (UpdateNodeExpression.Request location nodeId _)                      = do
        oldExpr <- Graph.withGraph location $ runASTOp $ GraphUtils.getASTTarget nodeId >>= Print.printNodeExpression
        return $ UpdateNodeExpression.Inverse (Text.pack oldExpr)
    action  (UpdateNodeExpression.Request location nodeId expression)             = Graph.updateNodeExpression location nodeId expression
    success (Request _ _ (UpdateNodeExpression.Request location nodeId _)) _ node = sendToBus' $ NodesUpdate.Update location [node]

handleUpdateNodeMeta :: Request UpdateNodeMeta.Request -> StateT Env BusT ()
handleUpdateNodeMeta = modifyGraphOk inverse action success where
    inverse (UpdateNodeMeta.Request location updates)     = do
        allNodes <- Graph.withGraph location $ runASTOp buildNodes
        let idSet = Set.fromList $ map fst updates
            prevMeta = catMaybes $ flip map allNodes $ \node ->
                if Set.member (node ^. Node.nodeId) idSet then
                     Just (node ^. Node.nodeId, node ^. Node.nodeMeta)
                else Nothing
        return $ UpdateNodeMeta.Inverse prevMeta
    action  (UpdateNodeMeta.Request location updates)     = forM_ updates $ uncurry $ Graph.updateNodeMeta location
    success (UpdateNodeMeta.Request location updates) _ _ = sendToBus' $ UpdateNodeMeta.Update location updates

handleRenameNode :: Request RenameNode.Request -> StateT Env BusT ()
handleRenameNode = modifyGraphOk inverse action success where
    inverse (RenameNode.Request location nodeId name)     = do
        prevName <- Graph.withGraph location $ runASTOp $ getNodeName nodeId
        return $ RenameNode.Inverse $ maybe "" id  prevName
    action  (RenameNode.Request location nodeId name)     = Graph.renameNode location nodeId name
    success (RenameNode.Request location nodeId name) _ _ = sendToBus' $ RenameNode.Update location nodeId name

handleSetCode :: Request SetCode.Request -> StateT Env BusT ()
handleSetCode = modifyGraphOk inverse action success where --FIXME[pm] implement this!
    inverse (SetCode.Request location nodeId code)     = do
        oldCode <- Graph.withGraph location $ runASTOp $ getNodeName nodeId
        return $ SetCode.Inverse $ fromMaybe def oldCode
    action  (SetCode.Request location nodeId code)     = void $ Graph.updateNodeExpression location nodeId code
    success (SetCode.Request location nodeId code) _ _ = sendToBus' $ SetCode.Update location nodeId code

handleMovePort :: Request MovePort.Request -> StateT Env BusT ()
handleMovePort = modifyGraphOk defInverse action success where
    action (MovePort.Request location portRef newPortRef) = void $ Graph.movePort location portRef newPortRef
    success _ _ _                                         = return ()

handleRenamePort :: Request RenamePort.Request -> StateT Env BusT ()
handleRenamePort = modifyGraphOk inverse action success where --FIXME[pm] implement this!
    inverse (RenamePort.Request location portRef name)     = do
        let oldName = "oldname" --FIXME
        return $ RenamePort.Inverse oldName
    action  (RenamePort.Request location portRef name)     = void $ Graph.renamePort location portRef name
    success (RenamePort.Request location portRef name) _ _ = sendToBus' $ RenamePort.Update location portRef name

handleRemovePort :: Request RemovePort.Request -> StateT Env BusT ()
handleRemovePort = modifyGraphOk defInverse action success where
    action (RemovePort.Request location portRef) = void $ Graph.removePort location portRef
    success _ _ _                                = return ()

handleSetDefaultValue :: Request SetDefaultValue.Request -> StateT Env BusT ()
handleSetDefaultValue = modifyGraphOk inverse action success where
    -- TODO[MM]: Get old defaultValue for inverse
    inverse                                                        = $notImplemented
    action (SetDefaultValue.Request location portRef defaultValue) = Graph.setDefaultValue location portRef defaultValue
    success _ _ _                                                  = return ()

stdlibFunctions :: [String]
stdlibFunctions = ["mockFunction"]

stdlibMethods :: [String]
stdlibMethods = ["mockMethod"]

handleGetProgram :: Request GetProgram.Request -> StateT Env BusT ()
handleGetProgram = modifyGraph defInverse action success where
    action (GetProgram.Request location) = do
        graph <- Graph.getGraph location
        code <-  Graph.getCode location
        crumb <- Graph.decodeLocation location
        return $ GetProgram.Result graph (Text.pack code) crumb mockNSData
    success req inv res                  = replyResult req inv res

handleNodeSearch :: Request NodeSearch.Request -> StateT Env BusT ()
handleNodeSearch = modifyGraph defInverse action success where
    action (NodeSearch.Request query cursor location) = return $ NodeSearch.Result mockNSData
    success req inv res                               = replyResult req inv res

handleDumpGraphViz :: Request DumpGraphViz.Request -> StateT Env BusT ()
handleDumpGraphViz (Request _ _ request) = do
    let location = request ^. DumpGraphViz.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.dumpGraphViz location

handleTypecheck :: Request TypeCheck.Request -> StateT Env BusT ()
handleTypecheck req@(Request _ _ request) = do
    let location = request ^. TypeCheck.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.typecheck location
    case result of
        Left err -> replyFail logger err req (Response.Error err)
        Right _  -> Env.empireEnv .= newEmpireEnv
    return ()

mockNSData :: NS.Items Node
mockNSData = Map.fromList $ functionsList <> modulesList where
    nodeSearcherSymbols = words "mockNode1 mockNode2 mockNode3 mockNode4"
    (methods, functions) = partition (elem '.') nodeSearcherSymbols
    functionsList = functionEntry <$> functions
    functionEntry function = (convert function, NS.Element $ mockNode "mockNode")
    modulesMethodsMap = foldl updateModulesMethodsMap Map.empty methods
    updateModulesMethodsMap map el = Map.insert moduleName methodNames map where
        (moduleName, dotMethodName) = break (== '.') el
        methodName = tail dotMethodName
        methodNames = methodName : (fromMaybe [] $ Map.lookup moduleName map)
    modulesList = (uncurry moduleEntry) <$> Map.toList modulesMethodsMap
    moduleEntry moduleName methodList = (convert moduleName, NS.Group (Map.fromList $ functionEntry <$> methodList) $ mockNode "mockGroupNode")
    mockNode name = Node (fromJust $ UUID.fromString "094f9784-3f07-40a1-84df-f9cf08679a27") name (Node.ExpressionNode name) False mockPorts def def
    mockPorts = Map.fromList [ (InPortId  Self  , Port (InPortId  Self)    "self" TStar NotConnected)
                             , (InPortId (Arg 0), Port (InPortId (Arg 0)) "arg 0" TStar NotConnected)
                             , (OutPortId All   , Port (OutPortId All)   "Output" TStar NotConnected)
                             ]
