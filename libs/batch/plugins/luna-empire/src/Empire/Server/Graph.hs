{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Empire.Server.Graph where

import           Control.Monad.Error                (throwError)
import           Control.Monad.State                (StateT)
import qualified Data.Binary                        as Bin
import           Data.ByteString                    (ByteString)
import           Data.ByteString.Lazy               (fromStrict)
import qualified Data.IntMap                        as IntMap
import           Data.List                          (break, find, partition)
import           Data.List.Split                    (splitOneOf)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe, isJust, isNothing, maybeToList)
import qualified Data.Set                           as Set
import           Data.Text                          (stripPrefix)
import qualified Data.Text                          as Text
import           Data.Traversable                   (forM)
import           Data.UUID.Types                    (UUID)
import qualified Data.UUID.Types                    as UUID
import qualified Data.UUID.V4                       as UUID
import qualified Empire.API.Atom.GetBuffer          as GetBuffer
import qualified Empire.API.Atom.Substitute         as Substitute
import           Empire.API.Data.Breadcrumb         (Breadcrumb (..))
import qualified Empire.API.Data.Breadcrumb         as Breadcrumb
import           Empire.API.Data.Connection         as Connection
import           Empire.API.Data.Graph              (Graph (..))
import qualified Empire.API.Data.Graph              as GraphAPI
import           Empire.API.Data.GraphLocation      (GraphLocation)
import qualified Empire.API.Data.GraphLocation      as GraphLocation
import           Empire.API.Data.LabeledTree        (LabeledTree (LabeledTree))
import           Empire.API.Data.Node               (ExpressionNode (..), NodeId)
import qualified Empire.API.Data.Node               as Node
import           Empire.API.Data.NodeLoc            (NodeLoc (..))
import qualified Empire.API.Data.NodeLoc            as NodeLoc
import           Empire.API.Data.NodeMeta           (NodeMeta)
import qualified Empire.API.Data.NodeMeta           as NodeMeta
import qualified Empire.API.Data.NodeSearcher       as NS
import           Empire.API.Data.Port               (InPort (..), InPortIndex (..), OutPort (..), OutPortIndex (..), Port (..),
                                                     PortState (..), getPortNumber)
import qualified Empire.API.Data.Port               as Port
import           Empire.API.Data.PortDefault        (PortValue (..), VisualizationValue (..))
import           Empire.API.Data.PortRef            (InPortRef (..), OutPortRef (..))
import           Empire.API.Data.PortRef            as PortRef
import           Empire.API.Data.Position           (Position)
import           Empire.API.Data.TypeRep            (TypeRep (TStar))
import qualified Empire.API.Graph.AddConnection     as AddConnection
import qualified Empire.API.Graph.AddNode           as AddNode
import qualified Empire.API.Graph.AddPort           as AddPort
import qualified Empire.API.Graph.AddSubgraph       as AddSubgraph
import qualified Empire.API.Graph.AutolayoutNodes   as AutolayoutNodes
import qualified Empire.API.Graph.ConnectUpdate     as ConnectUpdate
import qualified Empire.API.Graph.DumpGraphViz      as DumpGraphViz
import qualified Empire.API.Graph.GetProgram        as GetProgram
import qualified Empire.API.Graph.GetSubgraphs      as GetSubgraphs
import qualified Empire.API.Graph.MovePort          as MovePort
import           Empire.API.Graph.NodeResultUpdate  (NodeValue (NodeValue))
import qualified Empire.API.Graph.NodeResultUpdate  as NodeResultUpdate
import qualified Empire.API.Graph.RemoveConnection  as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes       as RemoveNodes
import qualified Empire.API.Graph.RemovePort        as RemovePort
import qualified Empire.API.Graph.RenameNode        as RenameNode
import qualified Empire.API.Graph.RenamePort        as RenamePort
import qualified Empire.API.Graph.Request           as G
import qualified Empire.API.Graph.SearchNodes       as SearchNodes
import qualified Empire.API.Graph.SetNodeCode       as SetNodeCode
import qualified Empire.API.Graph.SetNodeExpression as SetNodeExpression
import qualified Empire.API.Graph.SetNodesMeta      as SetNodesMeta
import qualified Empire.API.Graph.SetPortDefault    as SetPortDefault
import qualified Empire.API.Graph.TypeCheck         as TypeCheck
import           Empire.API.Request                 (Request (..))
import qualified Empire.API.Response                as Response
import qualified Empire.API.Topic                   as Topic
import           Empire.ASTOp                       (runASTOp)
import qualified Empire.ASTOps.Print                as Print
import           Empire.Commands.Autolayout         (autolayoutNodes)
import qualified Empire.Commands.Graph              as Graph
import           Empire.Commands.GraphBuilder       (buildConnections, buildGraph, buildNodes, getNodeName)
import qualified Empire.Commands.GraphUtils         as GraphUtils
import qualified Empire.Commands.Persistence        as Persistence
import           Empire.Empire                      (Empire)
import qualified Empire.Empire                      as Empire
import           Empire.Env                         (Env)
import qualified Empire.Env                         as Env
import           Empire.Server.Server               (errorMessage, replyFail, replyOk, replyResult, sendToBus')
import           Prologue                           hiding (Item)
import           System.Environment                 (getEnv)
import           System.FilePath                    ((</>))
import qualified System.Log.MLogger                 as Logger
import           ZMQ.Bus.Trans                      (BusT (..))


logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

notifyNodeResultUpdate :: GraphLocation -> NodeId -> [VisualizationValue] -> Text -> StateT Env BusT ()
notifyNodeResultUpdate location nodeId values name = sendToBus' $ NodeResultUpdate.Update location nodeId (NodeValue name values) 42
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

defaultLibraryPath = "Main.luna"

webGUIHack :: G.GraphRequest req => req -> IO req
webGUIHack req = do
    lunaroot <- liftIO $ getEnv "LUNAROOT"
    let path = lunaroot </> "projects" </> defaultLibraryPath
        realLocation = req ^. G.location
        realFile     = realLocation ^. GraphLocation.filePath
        hackedReq    = if null realFile then req & G.location . GraphLocation.filePath .~ path
                                        else req
    return hackedReq

modifyGraph :: forall req inv res res'. (G.GraphRequest req, Response.ResponseResult req inv res') => (req -> Empire inv) -> (req -> Empire res) -> (Request req -> inv -> res -> StateT Env BusT ()) -> Request req -> StateT Env BusT ()
modifyGraph inverse action success origReq@(Request uuid guiID request') = do
    request          <- liftIO $ webGUIHack request'
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (inv', _)        <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ inverse request
    case inv' of
        Left err  -> replyFail logger err origReq (Response.Error err)
        Right inv -> do
            let invStatus = Response.Ok inv
            (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ action request
            case result of
                Left  err    -> replyFail logger err origReq invStatus
                Right result -> do
                    Env.empireEnv .= newEmpireEnv
                    success origReq inv result
                    saveCurrentProject $ request ^. G.location

modifyGraphOk :: forall req inv res . (Bin.Binary req, G.GraphRequest req, Response.ResponseResult req inv ()) => (req -> Empire inv) -> (req -> Empire res) -> Request req -> StateT Env BusT ()
modifyGraphOk inverse action = modifyGraph inverse action (\req@(Request uuid guiID request) inv _ -> replyOk req inv)

-- helpers

defInverse :: a -> Empire ()
defInverse = const $ return ()

generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

addExpressionNode :: GraphLocation -> NodeId -> Text -> NodeMeta -> Maybe NodeId -> Empire ExpressionNode
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
        inPort   = if exprCall `elem` stdlibFunctions then [Arg 0] else [Self]
        request  = Request reqId guiId $ AddConnection.Request location (Left $ OutPortRef (NodeLoc def srcNodeId) []) (Left . InPortRef' $ InPortRef (NodeLoc def dstNodeId) inPort)
        action (AddConnection.Request location (Left src) (Left dst)) = Graph.connectCondTC False location src dst
        --TODO[LJK]: There should be result, not update
        success _ _ connection                                        = sendToBus' $ ConnectUpdate.Update location connection
    modifyGraph defInverse action success request
    forceTC location --TODO[MM]: is this not the same as: `Graph.connectCondTC True location src dst` in action???

-- Handlers


handleGetProgram :: Request GetProgram.Request -> StateT Env BusT ()
handleGetProgram = modifyGraph defInverse action replyResult where
    action (GetProgram.Request location) = do
        graph <- Graph.getGraph location
        code <-  Graph.getCode location
        crumb <- Graph.decodeLocation location
        return $ GetProgram.Result graph (Text.pack code) crumb mockNSData

handleAddConnection :: Request AddConnection.Request -> StateT Env BusT ()
handleAddConnection = modifyGraph defInverse action replyResult where
    getSrcPort src = case src of
        Left portRef -> portRef
        Right nodeId -> OutPortRef (NodeLoc def nodeId) []
    getDstPort dst = case dst of
        Left portRef  -> portRef
        Right nodeLoc -> InPortRef' $ InPortRef nodeLoc [Self]
    action  (AddConnection.Request location src' dst') = do
        let src = getSrcPort src'
            dst = getDstPort dst'
        conn  <- Graph.connectCondTC True location src dst
        graph <- Graph.getGraph location
        let allNodes = map Node.ExpressionNode' (graph ^. GraphAPI.nodes)
                    ++ map Node.InputSidebar'   (maybeToList $ graph ^. GraphAPI.inputSidebar)
                    ++ map Node.OutputSidebar'  (maybeToList $ graph ^. GraphAPI.outputSidebar)
            srcNode = find (\n -> n ^. Node.nodeId == src ^. PortRef.srcNodeId) allNodes
            dstNode = find (\n -> n ^. Node.nodeId == dst ^. PortRef.nodeId) allNodes
        if      isNothing srcNode then throwError "Connection source node not found"
        else if isNothing dstNode then throwError "Connection source node not found"
        else return $ AddConnection.Result conn (fromJust srcNode) (fromJust dstNode)

handleAddNode :: Request AddNode.Request -> StateT Env BusT ()
handleAddNode = modifyGraph defInverse action success where
    action (AddNode.Request location (NodeLoc _ nodeId) expression nodeMeta connectTo) = addExpressionNode location nodeId expression nodeMeta connectTo
    success req@(Request reqId guiId (AddNode.Request location (NodeLoc _ nodeId) expression _ connectTo)) inv node = do
        replyResult req inv node
        withJust connectTo $ connectNodes reqId guiId location expression nodeId

handleAddPort :: Request AddPort.Request -> StateT Env BusT ()
handleAddPort = modifyGraph defInverse action replyResult where
    action  (AddPort.Request location (OutPortRef (NodeLoc _ nid) (Projection i : _)) connections) = do
        case connections of
            Nothing -> Graph.addPort location nid i
            Just conns -> Graph.addPortWithConnection location nid i conns

handleAddSubgraph :: Request AddSubgraph.Request -> StateT Env BusT ()
handleAddSubgraph = modifyGraph defInverse action replyResult where
    action (AddSubgraph.Request location nodes connections) = Graph.addSubgraph location nodes connections

handleAutolayoutNodes :: Request AutolayoutNodes.Request -> StateT Env BusT ()
handleAutolayoutNodes = modifyGraph inverse action replyResult where
    action (AutolayoutNodes.Request location nodeLocs) = do
        Graph nodes connections _ _ _ <- Graph.getGraph location
        let updates = autolayoutNodes (convert <$> nodeLocs) nodes connections --TODO[PM -> MM] Use NodeLoc instead of NodeId
        mapM_ (uncurry $ Graph.setNodePosition location) updates
        let nidToNlMap = Map.fromList $ map (\nl -> (nl ^. NodeLoc.nodeId, nl)) nodeLocs
        return . catMaybes . map (\(nid, meta) -> (, meta) <$> Map.lookup nid nidToNlMap) $ updates
    inverse (AutolayoutNodes.Request location nodeLocs) = do
        let getNlAndPos :: NodeLoc -> Empire (Maybe (NodeLoc, Position))
            getNlAndPos nl = do
                mayMeta <- Graph.getNodeMeta location $ convert nl --TODO[PM -> MM] Use NodeLoc instead of NodeId
                return $ (nl,) . view NodeMeta.position <$> mayMeta
        AutolayoutNodes.Inverse . catMaybes <$> mapM getNlAndPos nodeLocs

handleDumpGraphViz :: Request DumpGraphViz.Request -> StateT Env BusT ()
handleDumpGraphViz = modifyGraphOk defInverse action where
    action (DumpGraphViz.Request location) = Graph.dumpGraphViz location

handleGetSubgraphs :: Request GetSubgraphs.Request -> StateT Env BusT ()
handleGetSubgraphs = modifyGraph defInverse action replyResult where
    action (GetSubgraphs.Request location) = do
        graph <- Graph.getGraph location
        return $ GetSubgraphs.Result $ Map.singleton (location ^. GraphLocation.breadcrumb . Breadcrumb.items . to last) graph --FIXME: should return multiple graphs

handleMovePort :: Request MovePort.Request -> StateT Env BusT ()
handleMovePort = modifyGraph defInverse action replyResult where
    action (MovePort.Request location portRef newPortPos) = Graph.movePort location portRef newPortPos

handleRemoveConnection :: Request RemoveConnection.Request -> StateT Env BusT ()
handleRemoveConnection = modifyGraphOk inverse action where
    inverse (RemoveConnection.Request location dst) = do
        connections <- Graph.withGraph location $ runASTOp buildConnections
        case find (\conn -> snd conn == dst) connections of
            Nothing       -> throwError "Connection does not exist"
            Just (src, _) -> return $ RemoveConnection.Inverse src
    action  (RemoveConnection.Request location dst) = Graph.disconnect location dst

handleRemoveNodes :: Request RemoveNodes.Request -> StateT Env BusT ()
handleRemoveNodes = modifyGraphOk inverse action where
    inverse (RemoveNodes.Request location nodeLocs) = do
        let nodeIds = convert <$> nodeLocs --TODO[PM -> MM] Use NodeLoc instead of NodeId
        Graph allNodes allConnections _ _ monads <- Graph.withGraph location $ runASTOp buildGraph
        let idSet = Set.fromList nodeIds
            nodes = flip filter allNodes       $ \node ->   Set.member (node ^. Node.nodeId)            idSet
            conns = flip filter allConnections $ \conn -> ( Set.member (conn ^. _1 . PortRef.srcNodeId) idSet
                                                         || Set.member (conn ^. _2 . PortRef.dstNodeId) idSet )
        return $ RemoveNodes.Inverse nodes $ map (uncurry Connection) conns
    action (RemoveNodes.Request location nodeLocs)  = Graph.removeNodes location $ convert <$> nodeLocs --TODO[PM -> MM] Use NodeLoc instead of NodeId

handleRemovePort :: Request RemovePort.Request -> StateT Env BusT ()
handleRemovePort = modifyGraphOk inverse action where
    inverse (RemovePort.Request location portRef) = do
        Graph allNodes allConnections _ _ monads <- Graph.withGraph location $ runASTOp buildGraph
        let conns = flip filter allConnections $ (== portRef) . fst
        return $ RemovePort.Inverse $ map (uncurry Connection) conns
    action (RemovePort.Request location portRef)  = Graph.removePort location portRef

handleRenameNode :: Request RenameNode.Request -> StateT Env BusT ()
handleRenameNode = modifyGraphOk inverse action where
    inverse (RenameNode.Request location nodeId name) = do
        prevName <- Graph.withGraph location $ runASTOp $ getNodeName nodeId
        return $ RenameNode.Inverse $ maybe "" id  prevName
    action (RenameNode.Request location nodeId name)  = Graph.renameNode location nodeId name


handleRenamePort :: Request RenamePort.Request -> StateT Env BusT ()
handleRenamePort = modifyGraphOk inverse action where --FIXME[pm] implement this!
    inverse (RenamePort.Request location portRef name) = do
        let oldName = "oldname" --FIXME
        return $ RenamePort.Inverse oldName
    action (RenamePort.Request location portRef name)  = void $ Graph.renamePort location portRef name

handleSearchNodes :: Request SearchNodes.Request -> StateT Env BusT ()
handleSearchNodes = modifyGraph defInverse action replyResult where
    action  _ = return $ SearchNodes.Result mockNSData

handleSetNodeCode :: Request SetNodeCode.Request -> StateT Env BusT ()
handleSetNodeCode = modifyGraphOk inverse action where --FIXME[pm] implement this!
    inverse (SetNodeCode.Request location nodeId code) = do
        oldCode <- Graph.withGraph location $ runASTOp $ getNodeName nodeId
        return $ SetNodeCode.Inverse $ fromMaybe def oldCode
    action (SetNodeCode.Request location nodeId code)  = void $ Graph.setNodeExpression location nodeId code

handleSetNodeExpression :: Request SetNodeExpression.Request -> StateT Env BusT ()-- fixme [SB] returns Result with no new informations and change node expression has addNode+removeNodes
handleSetNodeExpression = modifyGraphOk inverse action where
    inverse (SetNodeExpression.Request location nodeId _)         = do
        oldExpr <- Graph.withGraph location $ runASTOp $ GraphUtils.getASTTarget nodeId >>= Print.printNodeExpression
        return $ SetNodeExpression.Inverse (Text.pack oldExpr)
    action (SetNodeExpression.Request location nodeId expression) = Graph.setNodeExpression location nodeId expression

handleSetNodesMeta :: Request SetNodesMeta.Request -> StateT Env BusT ()
handleSetNodesMeta = modifyGraphOk inverse action where
    inverse (SetNodesMeta.Request location updates) = do
        allNodes <- Graph.withGraph location $ runASTOp buildNodes
        let idSet = Set.fromList $ map fst updates
            prevMeta = catMaybes $ flip map allNodes $ \node ->
                if Set.member (node ^. Node.nodeId) idSet then
                     Just (node ^. Node.nodeId, node ^. Node.nodeMeta)
                else Nothing
        return $ SetNodesMeta.Inverse prevMeta
    action (SetNodesMeta.Request location updates) = forM_ updates $ uncurry $ Graph.setNodeMeta location

handleSetPortDefault :: Request SetPortDefault.Request -> StateT Env BusT ()
handleSetPortDefault = modifyGraphOk inverse action where
    inverse (SetPortDefault.Request location portRef _)            = SetPortDefault.Inverse <$> Graph.getPortDefault location portRef
    action  (SetPortDefault.Request location portRef defaultValue) = Graph.setPortDefault location portRef defaultValue

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

-- FIXME[MM]: it's wrong but it works
instance G.GraphRequest Substitute.Request where
    location = lens getter setter where
        getter (Substitute.Request file _ _ _ _) = GraphLocation.GraphLocation file (Breadcrumb [])
        setter (Substitute.Request _    s e n c) (GraphLocation.GraphLocation file _) = Substitute.Request file s e n c

instance G.GraphRequest GetBuffer.Request where
    location = lens getter setter where
        getter (GetBuffer.Request file _) = GraphLocation.GraphLocation file (Breadcrumb [])
        setter (GetBuffer.Request _    s) (GraphLocation.GraphLocation file _) = GetBuffer.Request file s

handleSubstitute :: Request Substitute.Request -> StateT Env BusT ()
handleSubstitute = modifyGraph defInverse action success where
    action req@(Substitute.Request file start end newText cursor) = do
        Graph.substituteCode file start end newText cursor
        let loc = req ^. G.location
        graph <- Graph.getGraph loc
        code  <- Graph.getCode loc
        crumb <- Graph.decodeLocation loc
        return $ GetProgram.Result graph (Text.pack code) crumb mockNSData
    success (Request uuid guiID request) inv res = do
        -- DISCLAIMER, FIXME[MM]: ugly hack - send response to bogus GetProgram request
        -- after each substitute
        let loc = request ^. G.location
        replyResult (Request uuid guiID (GetProgram.Request loc)) () res

handleGetBuffer :: Request GetBuffer.Request -> StateT Env BusT ()
handleGetBuffer = modifyGraph defInverse action replyResult where
    action (GetBuffer.Request file span) = do
        code <- Graph.getBuffer file (head <$> span)
        return $ GetBuffer.Result code



stdlibFunctions :: [String]
stdlibFunctions = ["mockFunction"]

stdlibMethods :: [String]
stdlibMethods = ["mockMethod"]

mockNSData :: NS.Items ExpressionNode
-- mockNSData = Map.empty
mockNSData = Map.fromList $ functionsList <> modulesList where
    nodeSearcherSymbols = words "mockNodeA mockNodeB mockNodeC mockNodeD"
    (methods, functions) = partition (elem '.') nodeSearcherSymbols
    functionsList = functionEntry <$> functions
    functionEntry function = (convert function, NS.Element $ mockNode $ Just "mockNode")
    modulesMethodsMap = foldl updateModulesMethodsMap Map.empty methods
    updateModulesMethodsMap map el = Map.insert moduleName methodNames map where
        (moduleName, dotMethodName) = break (== '.') el
        methodName = tail dotMethodName
        methodNames = methodName : (fromMaybe [] $ Map.lookup moduleName map)
    modulesList = (uncurry moduleEntry) <$> Map.toList modulesMethodsMap
    moduleEntry moduleName methodList = (convert moduleName, NS.Group (Map.fromList $ functionEntry <$> methodList) $ mockNode $ Just "mockGroupNode")
    mockNode name = Node.ExpressionNode (fromJust $ UUID.fromString "094f9784-3f07-40a1-84df-f9cf08679a27") "" name Nothing mockInPorts mockOutPorts def False
    mockInPorts  = LabeledTree def $ Port.Port [Port.Arg 0]        "" TStar NotConnected
    mockOutPorts = LabeledTree def $ Port.Port [Port.Projection 0] "" TStar NotConnected
