{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Empire.Server.Graph where

import           Control.Arrow                          ((&&&))
import           Control.Concurrent.MVar                (readMVar)
import           Control.Monad.Catch                    (handle, try)
import           Control.Monad.State                    (StateT)
import           Control.Monad.Reader                   (asks)
import qualified Data.Binary                            as Bin
import           Data.ByteString                        (ByteString)
import           Data.ByteString.Lazy                   (fromStrict)
import qualified Data.IntMap                            as IntMap
import           Data.List                              (break, find, partition)
import           Data.List.Split                        (splitOneOf)
import qualified Data.Map                               as Map
import           Data.Maybe                             (fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import qualified Data.Set                               as Set
import           Data.Text                              (stripPrefix)
import qualified Data.Text                              as Text
import           Data.Traversable                       (forM)
import           Data.UUID.Types                        (UUID)
import qualified Data.UUID.Types                        as UUID
import qualified Data.UUID.V4                           as UUID
import           Empire.ASTOp                           (runASTOp)
import qualified Empire.ASTOps.Print                    as Print
import           Empire.Commands.Autolayout             (autolayoutNodes)
import qualified Empire.Commands.Graph                  as Graph
import           Empire.Commands.GraphBuilder           (buildConnections, buildGraph, buildNodes, getNodeName)
import qualified Empire.Commands.GraphUtils             as GraphUtils
import qualified Empire.Commands.Persistence            as Persistence
import           Empire.Data.AST                        (SomeASTException, astExceptionFromException, astExceptionToException)
import           Empire.Data.AST                        (SomeASTException)
import           Empire.Empire                          (Empire)
import qualified Empire.Empire                          as Empire
import           Empire.Env                             (Env)
import qualified Empire.Env                             as Env
import           Empire.Server.Server                   (errorMessage, replyFail, replyOk, replyResult, sendToBus')
import qualified LunaStudio.API.Atom.GetBuffer          as GetBuffer
import qualified LunaStudio.API.Atom.Substitute         as Substitute
import qualified LunaStudio.API.Graph.AddConnection     as AddConnection
import qualified LunaStudio.API.Graph.AddNode           as AddNode
import qualified LunaStudio.API.Graph.AddPort           as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph       as AddSubgraph
import qualified LunaStudio.API.Graph.AutolayoutNodes   as AutolayoutNodes
import qualified LunaStudio.API.Graph.DumpGraphViz      as DumpGraphViz
import qualified LunaStudio.API.Graph.GetProgram        as GetProgram
import qualified LunaStudio.API.Graph.GetSubgraphs      as GetSubgraphs
import qualified LunaStudio.API.Graph.MovePort          as MovePort
import           LunaStudio.API.Graph.NodeResultUpdate  (NodeValue (NodeValue))
import qualified LunaStudio.API.Graph.NodeResultUpdate  as NodeResultUpdate
import qualified LunaStudio.API.Graph.RemoveConnection  as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes       as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort        as RemovePort
import qualified LunaStudio.API.Graph.RenameNode        as RenameNode
import qualified LunaStudio.API.Graph.RenamePort        as RenamePort
import qualified LunaStudio.API.Graph.Request           as G
import qualified LunaStudio.API.Graph.Result            as Result
import qualified LunaStudio.API.Graph.SearchNodes       as SearchNodes
import qualified LunaStudio.API.Graph.SetNodeExpression as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta      as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault    as SetPortDefault
import qualified LunaStudio.API.Graph.TypeCheck         as TypeCheck
import           LunaStudio.API.Request                 (Request (..))
import qualified LunaStudio.API.Response                as Response
import qualified LunaStudio.API.Topic                   as Topic
import           LunaStudio.Data.Breadcrumb             (Breadcrumb (..))
import qualified LunaStudio.Data.Breadcrumb             as Breadcrumb
import           LunaStudio.Data.Connection             as Connection
import           LunaStudio.Data.Graph                  (Graph (..))
import qualified LunaStudio.Data.Graph                  as GraphAPI
import           LunaStudio.Data.GraphLocation          (GraphLocation)
import qualified LunaStudio.Data.GraphLocation          as GraphLocation
import           LunaStudio.Data.LabeledTree            (LabeledTree (LabeledTree))
import           LunaStudio.Data.Node                   (ExpressionNode (..), NodeId)
import qualified LunaStudio.Data.Node                   as Node
import           LunaStudio.Data.NodeLoc                (NodeLoc (..))
import qualified LunaStudio.Data.NodeLoc                as NodeLoc
import           LunaStudio.Data.NodeMeta               (NodeMeta)
import qualified LunaStudio.Data.NodeMeta               as NodeMeta
import qualified LunaStudio.Data.NodeSearcher           as NS
import           LunaStudio.Data.Port                   (InPort (..), InPortIndex (..), OutPort (..), OutPortIndex (..), Port (..),
                                                         PortState (..), getPortNumber)
import qualified LunaStudio.Data.Port                   as Port
import           LunaStudio.Data.PortDefault            (PortValue (..), VisualizationValue (..))
import           LunaStudio.Data.PortRef                (InPortRef (..), OutPortRef (..))
import           LunaStudio.Data.PortRef                as PortRef
import           LunaStudio.Data.Position               (Position)
import           LunaStudio.Data.TypeRep                (TypeRep (TStar))
import           Prologue                               hiding (Item)
import           System.Environment                     (getEnv)
import           System.FilePath                        ((</>))
import qualified System.Log.MLogger                     as Logger
import           ZMQ.Bus.Trans                          (BusT (..))


logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

saveCurrentProject :: GraphLocation -> StateT Env BusT ()
saveCurrentProject loc = do
  currentEmpireEnv <- use Env.empireEnv
  empireNotifEnv   <- use Env.empireNotif
  projectRoot      <- use Env.projectRoot
  void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Persistence.saveLocation projectRoot loc

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
    inv'             <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ inverse request
    case inv' of
        Left (exc :: SomeASTException) ->
            let err = displayException exc in replyFail logger err origReq (Response.Error err)
        Right (inv, _) -> do
            let invStatus = Response.Ok inv
            result <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ action request
            case result of
                Left  (exc :: SomeASTException) ->
                    let err = displayException exc in replyFail logger err origReq invStatus
                Right (result, newEmpireEnv) -> do
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

getAllNodes :: GraphLocation -> Empire [Node.Node]
getAllNodes location = do
    graph <- Graph.getGraph location
    return $ map Node.ExpressionNode' (graph ^. GraphAPI.nodes)
          ++ map Node.InputSidebar'   (maybeToList $ graph ^. GraphAPI.inputSidebar)
          ++ map Node.OutputSidebar'  (maybeToList $ graph ^. GraphAPI.outputSidebar)

getNodesByIds :: GraphLocation -> [NodeId] -> Empire [Node.Node]
getNodesByIds location nids = filter (\n -> Set.member (n ^. Node.nodeId) nidsSet) <$> getAllNodes location where
    nidsSet = Set.fromList nids

getExpressionNodesByIds :: GraphLocation -> [NodeId] -> Empire [Node.ExpressionNode]
getExpressionNodesByIds location nids = filter (\n -> Set.member (n ^. Node.nodeId) nidsSet) <$> Graph.getNodes location where
    nidsSet = Set.fromList nids

constructResult :: GraphAPI.Graph -> GraphAPI.Graph -> Result.Result
constructResult oldGraph newGraph = Result.Result removedNodeIds removedConnIds updatedInGraph where
    updatedInGraph = GraphAPI.Graph updatedNodes updatedConns updatedInputSidebar updatedOutputSidebar []
    oldNodesMap    = Map.fromList . map (view Node.nodeId &&& id) $ oldGraph ^. GraphAPI.nodes
    newNodeIdsSet  = Set.fromList . map (view Node.nodeId) $ newGraph ^. GraphAPI.nodes
    removedNodeIds = filter (flip Set.notMember newNodeIdsSet) $ Map.keys oldNodesMap
    updatedNodes   = filter (\n -> Just n /= Map.lookup (n ^. Node.nodeId) oldNodesMap) $ newGraph ^. GraphAPI.nodes
    oldConnsMap    = Map.fromList . map (snd &&& id) $ oldGraph ^. GraphAPI.connections
    newConnIdsSet  = Set.fromList . map snd $ newGraph ^. GraphAPI.connections
    removedConnIds = filter (flip Set.notMember newConnIdsSet) $ Map.keys oldConnsMap
    updatedConns   = filter (\c@(_, dst) -> Just c /= Map.lookup dst oldConnsMap) $ newGraph ^. GraphAPI.connections
    updatedInputSidebar = if oldGraph ^. GraphAPI.inputSidebar /= newGraph ^. GraphAPI.inputSidebar
        then newGraph ^. GraphAPI.inputSidebar else Nothing
    updatedOutputSidebar = if oldGraph ^. GraphAPI.outputSidebar /= newGraph ^. GraphAPI.outputSidebar
        then newGraph ^. GraphAPI.outputSidebar else Nothing

withDefaultResult :: GraphLocation -> Empire a -> Empire Result.Result
withDefaultResult location action = do
    oldGraph <- Graph.getGraphNoTC location
    void action
    constructResult oldGraph <$> Graph.getGraphNoTC location

getNodeById :: GraphLocation -> NodeId -> Empire (Maybe Node.Node)
getNodeById location nid = fmap listToMaybe $ getNodesByIds location [nid]

getSrcPortByNodeId :: NodeId -> OutPortRef
getSrcPortByNodeId nid = OutPortRef (NodeLoc def nid) []

getDstPortByNodeLoc :: NodeLoc -> AnyPortRef
getDstPortByNodeLoc nl = InPortRef' $ InPortRef nl [Self]

-- Handlers


handleGetProgram :: Request GetProgram.Request -> StateT Env BusT ()
handleGetProgram = modifyGraph defInverse action replyResult where
    action (GetProgram.Request location) = do
        graph <- Graph.getGraph location
        code  <- Graph.getCode location
        crumb <- Graph.decodeLocation location
        -- LESZEK! Tutaj pobieramy scope!
        d <- view Empire.scopeVar
        print =<< liftIO (readMVar d)
        return $ GetProgram.Result graph (Text.pack code) crumb mockNSData

handleAddConnection :: Request AddConnection.Request -> StateT Env BusT ()
handleAddConnection = modifyGraph inverse action replyResult where
    getSrcPort = either id getSrcPortByNodeId
    getDstPort = either id getDstPortByNodeLoc
    inverse (AddConnection.Request _ _ dst') = return . AddConnection.Inverse $
        case getDstPort dst' of
            InPortRef'  portRef -> portRef
            OutPortRef' portRef -> InPortRef (portRef ^. PortRef.nodeLoc) []
    action  (AddConnection.Request location src' dst') = withDefaultResult location $
        Graph.connectCondTC True location (getSrcPort src') (getDstPort dst')

handleAddNode :: Request AddNode.Request -> StateT Env BusT ()
handleAddNode = modifyGraph defInverse action replyResult where
    action (AddNode.Request location nl@(NodeLoc _ nodeId) expression nodeMeta connectTo) = withDefaultResult location $ do
        Graph.addNodeCondTC False location nodeId expression nodeMeta
        forM_ connectTo $ \nid -> do
            handle (\(e :: SomeASTException) -> return ()) $ do
                void $ Graph.connectCondTC False location (getSrcPortByNodeId nid) (getDstPortByNodeLoc nl)
                Graph.autolayoutNodes location [nodeId]
        Graph.typecheck location

handleAddPort :: Request AddPort.Request -> StateT Env BusT ()
handleAddPort = modifyGraph defInverse action replyResult where
    action (AddPort.Request location portRef connsDst) = withDefaultResult location $
        Graph.addPortWithConnections location portRef connsDst

handleAddSubgraph :: Request AddSubgraph.Request -> StateT Env BusT ()
handleAddSubgraph = modifyGraph defInverse action replyResult where
    action (AddSubgraph.Request location nodes connections) = withDefaultResult location $
        Graph.addSubgraph location nodes connections

handleAutolayoutNodes :: Request AutolayoutNodes.Request -> StateT Env BusT ()
handleAutolayoutNodes = modifyGraph inverse action replyResult where
    inverse (AutolayoutNodes.Request location nodeLocs) = do
        let getNlAndPos :: NodeLoc -> Empire (Maybe (NodeLoc, Position))
            getNlAndPos nl = do
                mayMeta <- Graph.getNodeMeta location $ convert nl --TODO[PM -> MM] Use NodeLoc instead of NodeId
                return $ (nl,) . view NodeMeta.position <$> mayMeta
        AutolayoutNodes.Inverse . catMaybes <$> mapM getNlAndPos nodeLocs
    action (AutolayoutNodes.Request location nodeLocs) = withDefaultResult location $
        Graph.autolayoutNodes location (convert <$> nodeLocs) --TODO[PM -> MM] Use NodeLoc instead of NodeId

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
    action (MovePort.Request location portRef newPortPos) = withDefaultResult location $
        Graph.movePort location portRef newPortPos

data ConnectionDoesNotExistException = ConnectionDoesNotExistException InPortRef
    deriving (Show)

instance Exception ConnectionDoesNotExistException where
    fromException = astExceptionFromException
    toException = astExceptionToException

data DestinationDoesNotExistException = DestinationDoesNotExistException InPortRef
    deriving (Show)

instance Exception DestinationDoesNotExistException where
    fromException = astExceptionFromException
    toException = astExceptionToException

handleRemoveConnection :: Request RemoveConnection.Request -> StateT Env BusT ()
handleRemoveConnection = modifyGraph inverse action replyResult where
    inverse (RemoveConnection.Request location dst) = do
        connections <- Graph.withGraph location $ runASTOp buildConnections
        case find (\conn -> snd conn == dst) connections of
            Nothing       -> throwM $ ConnectionDoesNotExistException dst
            Just (src, _) -> return $ RemoveConnection.Inverse src
    action (RemoveConnection.Request location dst) = withDefaultResult location $ do
        mayDstNode <- getNodeById location $ dst ^. PortRef.dstNodeId
        when (isNothing mayDstNode) $ throwM $ DestinationDoesNotExistException dst
        Graph.disconnect location dst

handleRemoveNodes :: Request RemoveNodes.Request -> StateT Env BusT ()
handleRemoveNodes = modifyGraph inverse action replyResult where
    inverse (RemoveNodes.Request location nodeLocs) = do
        let nodeIds = convert <$> nodeLocs --TODO[PM -> MM] Use NodeLoc instead of NodeId
        Graph allNodes allConnections _ _ monads <- Graph.withGraph location $ runASTOp buildGraph
        let idSet = Set.fromList nodeIds
            nodes = flip filter allNodes       $ \node ->   Set.member (node ^. Node.nodeId)            idSet
            conns = flip filter allConnections $ \conn -> ( Set.member (conn ^. _1 . PortRef.srcNodeId) idSet
                                                         || Set.member (conn ^. _2 . PortRef.dstNodeId) idSet )
        return $ RemoveNodes.Inverse nodes $ map (uncurry Connection) conns
    action (RemoveNodes.Request location nodeLocs) = withDefaultResult location $
        Graph.removeNodes location $ convert <$> nodeLocs --TODO[PM -> MM] Use NodeLoc instead of NodeId

data SidebarDoesNotExistException = SidebarDoesNotExistException
    deriving (Show)

instance Exception SidebarDoesNotExistException where
    fromException = astExceptionFromException
    toException = astExceptionToException

handleRemovePort :: Request RemovePort.Request -> StateT Env BusT ()
handleRemovePort = modifyGraph inverse action replyResult where
    inverse (RemovePort.Request location portRef) = do
        Graph allNodes allConnections _ _ monads <- Graph.withGraph location $ runASTOp buildGraph
        let conns = flip filter allConnections $ (== portRef) . fst
        return $ RemovePort.Inverse $ map (uncurry Connection) conns
    action (RemovePort.Request location portRef) = withDefaultResult location $ do
        maySidebar <- view GraphAPI.inputSidebar <$> Graph.getGraph location
        when (isNothing maySidebar) $ throwM SidebarDoesNotExistException
        Graph.removePort location portRef

handleRenameNode :: Request RenameNode.Request -> StateT Env BusT ()
handleRenameNode = modifyGraph inverse action replyResult where
    inverse (RenameNode.Request location nodeId name) = do
        prevName <- Graph.withGraph location $ runASTOp $ getNodeName nodeId
        return $ RenameNode.Inverse $ maybe "" id  prevName
    action (RenameNode.Request location nodeId name) = withDefaultResult location $
        Graph.renameNode location nodeId name

handleRenamePort :: Request RenamePort.Request -> StateT Env BusT ()
handleRenamePort = modifyGraph inverse action replyResult where --FIXME[pm] implement this!
    inverse (RenamePort.Request location portRef name) = do
        let oldName = "oldname" --FIXME
        return $ RenamePort.Inverse oldName
    action (RenamePort.Request location portRef name) = withDefaultResult location $
        Graph.renamePort location portRef name

handleSearchNodes :: Request SearchNodes.Request -> StateT Env BusT ()
handleSearchNodes = modifyGraph defInverse action replyResult where
    action  _ = return $ SearchNodes.Result mockNSData

handleSetNodeExpression :: Request SetNodeExpression.Request -> StateT Env BusT ()-- fixme [SB] returns Result with no new informations and change node expression has addNode+removeNodes
handleSetNodeExpression = modifyGraph inverse action replyResult where
    inverse (SetNodeExpression.Request location nodeId _) = do
        oldExpr <- Graph.withGraph location $ runASTOp $ GraphUtils.getASTTarget nodeId >>= Print.printNodeExpression
        return $ SetNodeExpression.Inverse (Text.pack oldExpr)
    action (SetNodeExpression.Request location nodeId expression) = withDefaultResult location $
        Graph.setNodeExpression location nodeId expression

handleSetNodesMeta :: Request SetNodesMeta.Request -> StateT Env BusT ()
handleSetNodesMeta = modifyGraph inverse action replyResult where
    inverse (SetNodesMeta.Request location updates) = do
        allNodes <- Graph.withGraph location $ runASTOp buildNodes
        let idSet = Set.fromList $ map fst updates
            prevMeta = catMaybes $ flip map allNodes $ \node ->
                if Set.member (node ^. Node.nodeId) idSet then
                     Just (node ^. Node.nodeId, node ^. Node.nodeMeta)
                else Nothing
        return $ SetNodesMeta.Inverse prevMeta
    action (SetNodesMeta.Request location updates) = withDefaultResult location $
        forM_ updates $ uncurry $ Graph.setNodeMeta location

handleSetPortDefault :: Request SetPortDefault.Request -> StateT Env BusT ()
handleSetPortDefault = modifyGraph inverse action replyResult where
    inverse (SetPortDefault.Request location portRef _)            = SetPortDefault.Inverse <$> Graph.getPortDefault location portRef
    action  (SetPortDefault.Request location portRef defaultValue) = withDefaultResult location $
        Graph.setPortDefault location portRef defaultValue

handleTypecheck :: Request TypeCheck.Request -> StateT Env BusT ()
handleTypecheck req@(Request _ _ request) = do
    let location = request ^. TypeCheck.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result           <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.typecheck location
    case result of
        Left (exc :: SomeASTException) -> let err = displayException exc in replyFail logger err req (Response.Error err)
        Right (_, newEmpireEnv) -> Env.empireEnv .= newEmpireEnv
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
    nodeSearcherSymbols = words "foo bar baz List.xD"
    (methods, functions) = partition (elem '.') nodeSearcherSymbols
    functionsList = functionEntry <$> functions
    functionEntry function = (convert function, NS.Element mockNode)
    modulesMethodsMap = foldl updateModulesMethodsMap Map.empty methods
    updateModulesMethodsMap map el = Map.insert moduleName methodNames map where
        (moduleName, dotMethodName) = break (== '.') el
        methodName = tail dotMethodName
        methodNames = methodName : (fromMaybe [] $ Map.lookup moduleName map)
    modulesList = (uncurry moduleEntry) <$> Map.toList modulesMethodsMap
    moduleEntry moduleName methodList = (convert moduleName, NS.Group (Map.fromList $ functionEntry <$> methodList) mockNode)
    mockNode = Node.ExpressionNode (fromJust $ UUID.fromString "094f9784-3f07-40a1-84df-f9cf08679a27") "" Nothing Nothing mockInPorts mockOutPorts def False
    mockInPorts  = LabeledTree def $ Port.Port [] "" TStar NotConnected
    mockOutPorts = LabeledTree def $ Port.Port [] "" TStar NotConnected
