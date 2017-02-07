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
import           Data.List                             (break, partition)
import           Data.List.Split                       (splitOneOf)
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromMaybe, isJust, isNothing)
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
import           Empire.API.Data.GraphLocation         (GraphLocation)
import           Empire.API.Data.Node                  (Node (..), NodeId)
import qualified Empire.API.Data.Node                  as Node
import           Empire.API.Data.NodeMeta              (NodeMeta)
import qualified Empire.API.Data.NodeSearcher          as NS
import           Empire.API.Data.Port                  (InPort (..), OutPort (..), Port (..), PortId (..), PortState (..))
import           Empire.API.Data.PortRef               (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.PortRef               as PortRef
import qualified Empire.API.Graph.AddNode              as AddNode
import qualified Empire.API.Graph.AddSubgraph          as AddSubgraph
import qualified Empire.API.Graph.CodeUpdate           as CodeUpdate
import qualified Empire.API.Graph.Connect              as Connect
import qualified Empire.API.Graph.Disconnect           as Disconnect
import qualified Empire.API.Graph.DumpGraphViz         as DumpGraphViz
import qualified Empire.API.Graph.GetProgram           as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate     as NodeResultUpdate
import qualified Empire.API.Graph.NodesUpdate          as NodesUpdate
import qualified Empire.API.Graph.RemoveNodes          as RemoveNodes
import qualified Empire.API.Graph.RenameNode           as RenameNode
import qualified Empire.API.Graph.Request              as G
import qualified Empire.API.Graph.SetDefaultValue      as SetDefaultValue
import qualified Empire.API.Graph.TypeCheck            as TypeCheck
import qualified Empire.API.Graph.UpdateNodeExpression as UpdateNodeExpression
import qualified Empire.API.Graph.UpdateNodeMeta       as UpdateNodeMeta
import           Empire.API.Request                    (Request (..))
import qualified Empire.API.Response                   as Response
import qualified Empire.API.Topic                      as Topic
import qualified Empire.ASTOps.Print                   as Print
import qualified Empire.Commands.Graph                 as Graph
import           Empire.Commands.GraphBuilder          (buildNodes, getNodeName, buildConnections)
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
        Right code -> sendToBus' $ CodeUpdate.Update location $ Text.pack code

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

modifyGraph :: forall req inv res d. (G.GraphRequest req, Response.ResponseResult req inv res ) => (req -> Empire (inv, res)) -> (Request req -> inv -> res -> StateT Env BusT ()) -> Request req -> StateT Env BusT ()
modifyGraph action success req@(Request uuid guiID request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ action request
    case result of
        Left err     -> replyFail logger err req
        Right (inv, result) -> do
            Env.empireEnv .= newEmpireEnv
            success req inv result
            notifyCodeUpdate $ request ^. G.location
            saveCurrentProject $ request ^. G.location

modifyGraphOk :: forall req inv res d. (Bin.Binary req, G.GraphRequest req, Response.ResponseResult req inv res, Response.ResponseResult req inv ()) => (req -> Empire (inv, res)) -> (req -> inv -> res -> StateT Env BusT ()) -> Request req -> StateT Env BusT ()
modifyGraphOk action success = modifyGraph action (\req@(Request uuid guiID request) inv res -> replyOk req inv >> success request inv res)

-- helpers

generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

addExpressionNode :: GraphLocation -> Text -> NodeMeta -> Maybe NodeId -> Maybe NodeId -> Empire Node
addExpressionNode location expression nodeMeta connectTo nodeId = do
    nId <- case nodeId of
        Nothing      -> liftIO generateNodeId
        Just nodeId' -> pure nodeId'

    case parseExpr expression of
        Expression expression -> do
            Graph.addNodeCondTC (isNothing connectTo) location nId expression nodeMeta
        Function (Just name) -> do
            Graph.addNodeCondTC False location nId (Text.append "def " name) nodeMeta
        Module   name -> throwError "Module Nodes not yet supported"
        Input    name -> throwError "Input Nodes not yet supported"
        Output   name -> throwError "Output Nodes not yet supported"


connectNodes :: GraphLocation -> Text -> NodeId -> NodeId -> StateT Env BusT ()
connectNodes location expr dstNodeId srcNodeId = do
    let exprCall = head $ splitOneOf " ." $ Text.unpack expr
        inPort = if exprCall `elem` stdlibFunctions then Arg 0 else Self
        connectRequest = Request UUID.nil Nothing $ Connect.Request location (Connect.PortConnection (OutPortRef srcNodeId All) (InPortRef dstNodeId inPort))
    handleConnectReq False connectRequest -- TODO: refactor (we should not call handlers from handlers)
    forceTC location

-- Handlers

mtuple :: (Monad m, Applicative m)=>(a -> m b) -> a -> m ((), b)
mtuple f a = f a >>= \b -> pure ((),b)

handleAddNode :: Request AddNode.Request -> StateT Env BusT ()
handleAddNode = modifyGraph (mtuple action) success where
    action (AddNode.Request location nodeType nodeMeta connectTo nodeId) = case nodeType of
        AddNode.ExpressionNode expression -> addExpressionNode location expression nodeMeta connectTo nodeId
    success request@(Request _ _ req@(AddNode.Request location nodeType nodeMeta connectTo nodeId)) _ node = do
        replyResult request () node
        sendToBus' $ AddNode.Update location node
        case nodeType of
            AddNode.ExpressionNode expr -> withJust connectTo $ connectNodes location expr (node ^. Node.nodeId)

handleAddSubgraph :: Request AddSubgraph.Request -> StateT Env BusT ()
handleAddSubgraph = modifyGraph (mtuple action) success where
    action (AddSubgraph.Request location nodes connections saveNodeIds) = Graph.addSubgraph location nodes connections saveNodeIds
    success request@(Request _ _ req@(AddSubgraph.Request location nodes connections saveNodeIds)) _ idMap   = do
        replyResult request () idMap

handleRemoveNodes :: Request RemoveNodes.Request -> StateT Env BusT ()
handleRemoveNodes = modifyGraphOk action success where
    action  (RemoveNodes.Request location nodeIds) = do
        allNodes    <- Graph.withGraph location buildNodes
        connections <- Graph.withGraph location buildConnections
        let inv = RemoveNodes.Inverse allNodes connections
        (inv,) <$> Graph.removeNodes location nodeIds
    success (RemoveNodes.Request location nodeIds) _ result = sendToBus' $ RemoveNodes.Update location nodeIds

handleUpdateNodeExpression :: Request UpdateNodeExpression.Request -> StateT Env BusT ()-- fixme [SB] returns Result with no new informations and change node expression has addNode+removeNodes
handleUpdateNodeExpression = modifyGraph action success where
    action (UpdateNodeExpression.Request location nodeId expression) = do
        oldExpr <- Graph.withGraph location $ $notImplemented -- GraphUtils.getASTTarget nodeId >>= Print.printNodeExpression
        let newNodeId = nodeId
            inv = UpdateNodeExpression.Inverse (Text.pack oldExpr)
            res = Graph.updateNodeExpression location nodeId newNodeId expression
        (inv,) <$> res
    success request@(Request _ _ req@(UpdateNodeExpression.Request location nodeId expression)) _ nodeMay = do
        withJust nodeMay $ \node -> do
            sendToBus' $ AddNode.Update location node
            sendToBus' $ RemoveNodes.Update location [nodeId]

handleUpdateNodeMeta :: Request UpdateNodeMeta.Request -> StateT Env BusT ()
handleUpdateNodeMeta = modifyGraphOk action success where
    action  (UpdateNodeMeta.Request location updates) = do
        allNodes <- Graph.withGraph location buildNodes
        let inv = UpdateNodeMeta.Inverse allNodes
            res = forM_ updates $ uncurry $ Graph.updateNodeMeta location
        (inv,) <$> res
    success (UpdateNodeMeta.Request location updates) _ result = sendToBus' $ UpdateNodeMeta.Update location updates

handleRenameNode :: Request RenameNode.Request -> StateT Env BusT ()
handleRenameNode = modifyGraphOk action success where
    action  (RenameNode.Request location nodeId name) = do
        oldName <- Graph.withGraph location $ getNodeName nodeId
        let inv = RenameNode.Inverse oldName
        (inv,) <$> Graph.renameNode location nodeId name
    success (RenameNode.Request location nodeId name) _ result = sendToBus' $ RenameNode.Update location nodeId name

handleConnect :: Request Connect.Request -> StateT Env BusT ()
handleConnect = handleConnectReq True

-- TODO: Response for this request needs more info in case of NodeConnection for undo/redo
handleConnectReq :: Bool -> Request Connect.Request -> StateT Env BusT ()
handleConnectReq doTC = modifyGraphOk (mtuple action) success where
    action  (Connect.Request location (Connect.PortConnection src dst)) = Graph.connectCondTC doTC location src dst
    action  (Connect.Request location (Connect.NodeConnection src dst)) = Graph.connectCondTC doTC location (OutPortRef src All) (InPortRef dst Self)
    success (Connect.Request location (Connect.PortConnection src dst)) _ result = sendToBus' $ Connect.Update location src dst
    success (Connect.Request location (Connect.NodeConnection src dst)) _ result = sendToBus' $ Connect.Update location (OutPortRef src All) (InPortRef dst Self)

handleDisconnect :: Request Disconnect.Request -> StateT Env BusT ()
handleDisconnect = modifyGraphOk action success where
    action  (Disconnect.Request location dst) = do
        connection <- Graph.withGraph location buildConnections
        let inv = Disconnect.Inverse $ fst $ head connection
        (inv,) <$> Graph.disconnect location dst
    success (Disconnect.Request location dst) _ result = sendToBus' $ Disconnect.Update location dst

handleSetDefaultValue :: Request SetDefaultValue.Request -> StateT Env BusT ()
handleSetDefaultValue = modifyGraphOk (mtuple action) success where
    action (SetDefaultValue.Request location portRef defaultValue) = Graph.setDefaultValue location portRef defaultValue
    success _ _ _ = return ()

stdlibFunctions :: [String]
stdlibFunctions = ["mockFunction"]

stdlibMethods :: [String]
stdlibMethods = ["mockMethod"]

handleGetProgram :: Request GetProgram.Request -> StateT Env BusT ()
handleGetProgram = modifyGraph (mtuple action) success where
    action (GetProgram.Request location) = do
        graph <- Graph.getGraph location
        code <-  Graph.getCode location
        crumb <- Graph.decodeLocation location
        return $ GetProgram.Result graph (Text.pack code) crumb mockNSData
    success req _ res = replyResult req () res

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
        Left err -> replyFail logger err req
        Right _  -> Env.empireEnv .= newEmpireEnv
    return ()

mockNSData :: NS.Items
mockNSData = Map.fromList $ functionsList <> modulesList where
    nodeSearcherSymbols = ["mockNodeSearcherSymbol"]
    (methods, functions) = partition (elem '.') nodeSearcherSymbols
    functionsList = functionEntry <$> functions
    functionEntry function = (convert function, NS.Element)
    modulesMethodsMap = foldl updateModulesMethodsMap Map.empty methods
    updateModulesMethodsMap map el = Map.insert moduleName methodNames map where
        (moduleName, dotMethodName) = break (== '.') el
        methodName = tail dotMethodName
        methodNames = methodName : (fromMaybe [] $ Map.lookup moduleName map)
    modulesList = (uncurry moduleEntry) <$> Map.toList modulesMethodsMap
    moduleEntry moduleName methodList = (convert moduleName, NS.Group $ Map.fromList $ functionEntry <$> methodList)
