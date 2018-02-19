{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Empire.Server.Graph where

import qualified Compress
import           Control.Arrow                           ((&&&))
import           Control.Concurrent                      (forkIO)
import           Control.Concurrent.MVar                 (readMVar)
import           Control.Concurrent.STM.TChan            (writeTChan)
import           Control.Error                           (runExceptT)
import           Control.Monad                           (when)
import           Control.Monad.Catch                     (handle, try)
import           Control.Monad.Reader                    (asks)
import           Control.Monad.State                     (StateT, evalStateT, get)
import           Control.Monad.STM                       (atomically)
import qualified Data.Binary                             as Bin
import           Data.ByteString                         (ByteString)
import           Data.ByteString.Lazy                    (fromStrict)
import           Data.Char                               (isUpper)
import qualified Data.HashMap.Strict                     as HashMap
import qualified Data.IntMap                             as IntMap
import           Data.List                               (break, find, partition, sortBy)
import           Data.List.Split                         (splitOneOf)
import qualified Data.Map                                as Map
import           Data.Maybe                              (isJust, isNothing, listToMaybe, maybeToList)
import qualified Data.Set                                as Set
import           Data.Text                               (stripPrefix)
import qualified Data.Text                               as Text
import           Data.Traversable                        (forM)
import           Data.UUID.Types                         (UUID)
import qualified Data.UUID.Types                         as UUID
import qualified Data.UUID.V4                            as UUID
import           Empire.ASTOp                            (runASTOp)
import qualified Empire.ASTOps.Print                     as Print
import           Empire.Commands.Autolayout              (autolayoutNodes)
import qualified Empire.Commands.Graph                   as Graph
import           Empire.Commands.GraphBuilder            (buildClassGraph, buildConnections, buildGraph, buildNodes, getNodeName)
import qualified Empire.Commands.GraphUtils              as GraphUtils
import           Empire.Data.AST                         (SomeASTException, astExceptionFromException, astExceptionToException)
import qualified Empire.Data.Graph                       as Graph (code, nodeCache)
import           Empire.Empire                           (Empire)
import qualified Empire.Empire                           as Empire
import           Empire.Env                              (Env)
import qualified Empire.Env                              as Env
import           Empire.Server.Server                    (defInverse, errorMessage, modifyGraph, modifyGraphOk, prettyException, replyFail,
                                                          replyOk, replyResult, sendToBus', webGUIHack, withDefaultResult,
                                                          withDefaultResultTC)
import           Luna.Project                            (findProjectFileForFile, getRelativePathForModule)
import qualified LunaStudio.API.Atom.GetBuffer           as GetBuffer
import qualified LunaStudio.API.Atom.Substitute          as Substitute
import qualified LunaStudio.API.Control.Interpreter      as Interpreter
import qualified LunaStudio.API.Graph.AddConnection      as AddConnection
import qualified LunaStudio.API.Graph.AddImports         as AddImports
import qualified LunaStudio.API.Graph.AddNode            as AddNode
import qualified LunaStudio.API.Graph.AddPort            as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph        as AddSubgraph
import qualified LunaStudio.API.Graph.AutolayoutNodes    as AutolayoutNodes
import qualified LunaStudio.API.Graph.CollapseToFunction as CollapseToFunction
import qualified LunaStudio.API.Graph.Copy               as Copy
import qualified LunaStudio.API.Graph.DumpGraphViz       as DumpGraphViz
import qualified LunaStudio.API.Graph.GetProgram         as GetProgram
import qualified LunaStudio.API.Graph.GetSubgraphs       as GetSubgraphs
import qualified LunaStudio.API.Graph.MovePort           as MovePort
import qualified LunaStudio.API.Graph.Paste              as Paste
import qualified LunaStudio.API.Graph.RemoveConnection   as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes        as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort         as RemovePort
import qualified LunaStudio.API.Graph.RenameNode         as RenameNode
import qualified LunaStudio.API.Graph.RenamePort         as RenamePort
import qualified LunaStudio.API.Graph.Request            as G
import qualified LunaStudio.API.Graph.Result             as Result
import qualified LunaStudio.API.Graph.SaveSettings       as SaveSettings
import qualified LunaStudio.API.Graph.SearchNodes        as SearchNodes
import qualified LunaStudio.API.Graph.SetCode            as SetCode
import qualified LunaStudio.API.Graph.SetNodeExpression  as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta       as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault     as SetPortDefault
import qualified LunaStudio.API.Graph.TypeCheck          as TypeCheck
import           LunaStudio.API.Request                  (Request (..))
import qualified LunaStudio.API.Response                 as Response
import qualified LunaStudio.API.Topic                    as Topic
import           LunaStudio.Data.Breadcrumb              (Breadcrumb (..))
import qualified LunaStudio.Data.Breadcrumb              as Breadcrumb
import qualified LunaStudio.Data.CameraTransformation    as Camera
import           LunaStudio.Data.Connection              as Connection
import           LunaStudio.Data.Diff                    (Diff (..))
import           LunaStudio.Data.Graph                   (Graph (..))
import qualified LunaStudio.Data.Graph                   as GraphAPI
import           LunaStudio.Data.GraphLocation           (GraphLocation (..))
import qualified LunaStudio.Data.GraphLocation           as GraphLocation
import           LunaStudio.Data.LabeledTree             (LabeledTree (LabeledTree))
import           LunaStudio.Data.Node                    (ExpressionNode (..), NodeId)
import qualified LunaStudio.Data.Node                    as Node
import           LunaStudio.Data.NodeLoc                 (NodeLoc (..))
import qualified LunaStudio.Data.NodeLoc                 as NodeLoc
import           LunaStudio.Data.NodeMeta                (NodeMeta)
import qualified LunaStudio.Data.NodeMeta                as NodeMeta
import qualified LunaStudio.Data.NodeSearcher            as NS
import           LunaStudio.Data.NodeValue               (NodeValue (NodeValue))
import           LunaStudio.Data.Port                    (InPort (..), InPortIndex (..), OutPort (..), OutPortIndex (..), Port (..),
                                                          PortState (..), getPortNumber)
import qualified LunaStudio.Data.Port                    as Port
import           LunaStudio.Data.PortDefault             (PortValue (..))
import           LunaStudio.Data.PortRef                 (InPortRef (..), OutPortRef (..))
import           LunaStudio.Data.PortRef                 as PortRef
import           LunaStudio.Data.Position                (Position)
import qualified LunaStudio.Data.Position                as Position
import           LunaStudio.Data.Project                 (LocationSettings)
import qualified LunaStudio.Data.Project                 as Project
import           LunaStudio.Data.TypeRep                 (TypeRep (TStar))
import           LunaStudio.Data.Visualization           (VisualizationValue (..))
import           Path                                    (fromAbsFile, fromRelFile, parseAbsFile)
import qualified Path                                    as Path
import           Prologue                                hiding (Item, when)
import qualified Safe                                    as Safe
import           System.Environment                      (getEnv)
import           System.FilePath                         (dropFileName, replaceFileName, (</>))
import qualified System.Log.MLogger                      as Logger
import qualified ZMQ.Bus.Bus                             as Bus
import qualified ZMQ.Bus.Config                          as Config
import qualified ZMQ.Bus.Data.Message                    as Message
import qualified ZMQ.Bus.EndPoint                        as EP
import           ZMQ.Bus.Trans                           (BusT (..))
import qualified ZMQ.Bus.Trans                           as BusT

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

logProjectPathNotFound :: MonadIO m => m ()
logProjectPathNotFound = Project.logProjectSettingsError "Could not find project path."
-- helpers


generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

getAllNodes :: GraphLocation -> Empire [Node.Node]
getAllNodes location = do
    graph <- Graph.getGraph location
    return $ map Node.ExpressionNode' (graph ^. GraphAPI.nodes)
          <> map Node.InputSidebar'   (maybeToList $ graph ^. GraphAPI.inputSidebar)
          <> map Node.OutputSidebar'  (maybeToList $ graph ^. GraphAPI.outputSidebar)

getNodesByIds :: GraphLocation -> [NodeId] -> Empire [Node.Node]
getNodesByIds location nids = filter (\n -> Set.member (n ^. Node.nodeId) nidsSet) <$> getAllNodes location where
    nidsSet = Set.fromList nids

getExpressionNodesByIds :: GraphLocation -> [NodeId] -> Empire [Node.ExpressionNode]
getExpressionNodesByIds location nids = filter (\n -> Set.member (n ^. Node.nodeId) nidsSet) <$> Graph.getNodes location where
    nidsSet = Set.fromList nids

getNodeById :: GraphLocation -> NodeId -> Empire (Maybe Node.Node)
getNodeById location nid = fmap listToMaybe $ getNodesByIds location [nid]

getSrcPortByNodeId :: NodeId -> OutPortRef
getSrcPortByNodeId nid = OutPortRef (NodeLoc def nid) []

getDstPortByNodeLoc :: NodeLoc -> AnyPortRef
getDstPortByNodeLoc nl = InPortRef' $ InPortRef nl [Self]

getProjectPathAndRelativeModulePath :: MonadIO m => FilePath -> m (Maybe (FilePath, FilePath))
getProjectPathAndRelativeModulePath modulePath = do
    let eitherToMaybe :: MonadIO m => Either Path.PathException (Path.Path Path.Abs Path.File) -> m (Maybe (Path.Path Path.Abs Path.File))
        eitherToMaybe (Left  e) = Project.logProjectSettingsError e >> return def
        eitherToMaybe (Right a) = return $ Just a
    mayProjectPathAndRelModulePath <- liftIO . runMaybeT $ do
        absModulePath  <- MaybeT $ eitherToMaybe =<< try (parseAbsFile modulePath)
        absProjectPath <- MaybeT $ findProjectFileForFile absModulePath
        relModulePath  <- MaybeT $ getRelativePathForModule absProjectPath absModulePath
        return (fromAbsFile absProjectPath, fromRelFile relModulePath)
    when (isNothing mayProjectPathAndRelModulePath) logProjectPathNotFound
    return mayProjectPathAndRelModulePath

saveSettings :: GraphLocation -> LocationSettings -> GraphLocation -> Empire ()
saveSettings gl settings newGl = handle (\(e :: SomeException) -> Project.logProjectSettingsError e) $ do
    bc    <- Breadcrumb.toNames <$> Graph.decodeLocation gl
    newBc <- Breadcrumb.toNames <$> Graph.decodeLocation newGl
    let filePath        = gl    ^. GraphLocation.filePath
        newFilePath     = newGl ^. GraphLocation.filePath
        lastBcInOldFile = if filePath == newFilePath then newBc else bc
    withJustM (getProjectPathAndRelativeModulePath filePath) $ \(cp, fp) ->
        Project.updateLocationSettings cp fp bc settings lastBcInOldFile
    when (filePath /= newFilePath) $ withJustM (getProjectPathAndRelativeModulePath newFilePath) $ \(cp, fp) ->
        Project.updateCurrentBreadcrumbSettings cp fp newBc

getClosestBcLocation :: GraphLocation -> Breadcrumb Text -> Empire GraphLocation
getClosestBcLocation gl (Breadcrumb []) = return gl
getClosestBcLocation gl (Breadcrumb (nodeName:newBcItems)) = do
    g <- Graph.getGraph gl
    let mayN = find ((Just nodeName ==) . view Node.name) (g ^. GraphAPI.nodes)
        processLocation n = do
            let nid = n ^. Node.nodeId
                bci = if n ^. Node.isDefinition then Breadcrumb.Definition nid else Breadcrumb.Lambda nid
                nextLocation = gl & GraphLocation.breadcrumb . Breadcrumb.items %~ (<>[bci])
            getClosestBcLocation nextLocation $ Breadcrumb newBcItems
    maybe (return gl) processLocation mayN


-- Handlers


handleGetProgram :: Request GetProgram.Request -> StateT Env BusT ()
handleGetProgram = modifyGraph defInverse action replyResult where
    action (GetProgram.Request location' mayPrevSettings retrieveLocation) = do
        let moduleChanged = isNothing mayPrevSettings || isJust (maybe Nothing (view Project.visMap . snd) mayPrevSettings)
        (graph, crumb, availableImports, typeRepToVisMap, camera, location, mayVisPath) <- handle
            (\(e :: SomeASTException) -> return (Left . Graph.prepareGraphError $ toException e, Breadcrumb [], def, mempty, def, location', def))
            $ do
                let filePath = location' ^. GraphLocation.filePath
                    closestBc loc bc = getClosestBcLocation (GraphLocation.GraphLocation filePath def) bc
                mayProjectPathAndRelModulePath <- liftIO $ getProjectPathAndRelativeModulePath filePath
                mayModuleSettings              <- liftIO $ maybe (return def) (uncurry Project.getModuleSettings) mayProjectPathAndRelModulePath
                location <- if not retrieveLocation then return location'
                    else getClosestBcLocation (GraphLocation.GraphLocation filePath def) $ maybe (Breadcrumb ["main"]) (view Project.currentBreadcrumb) mayModuleSettings
                graph            <- Graph.getGraph location
                crumb            <- Graph.decodeLocation location
                availableImports <- Graph.getAvailableImports location
                let mayVisPath    = ((</> "visualizers") . dropFileName . fst) <$> mayProjectPathAndRelModulePath
                    defaultCamera = maybe def (flip Camera.getCameraForRectangle def) . Position.minimumRectangle . map (view Node.position) $ graph ^. GraphAPI.nodes
                    (typeRepToVisMap, camera) = case mayModuleSettings of
                        Nothing -> (mempty, defaultCamera)
                        Just ms -> let visMap' = if moduleChanged then Just $ ms ^. Project.typeRepToVisMap else Nothing
                                       visMap  = fmap2 Project.fromOldAPI $ visMap'
                                       bc      = Breadcrumb.toNames crumb
                                       bs      = Map.lookup bc $ ms ^. Project.breadcrumbsSettings
                                       cam     = maybe defaultCamera (view Project.breadcrumbCameraSettings) bs
                            in (visMap, cam)
                return (Right graph, crumb, availableImports, typeRepToVisMap, camera, location, mayVisPath)
        code <- Graph.getCode location
        withJust mayPrevSettings $ \(gl, locSettings) -> saveSettings gl locSettings location
        return $ GetProgram.Result graph code crumb availableImports typeRepToVisMap camera location mayVisPath

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

handleAddImports :: Request AddImports.Request -> StateT Env BusT ()
handleAddImports = modifyGraph defInverse action replyResult where
    action (AddImports.Request location modules) = withDefaultResult location $
        Graph.addImports location modules

handleAddNode :: Request AddNode.Request -> StateT Env BusT ()
handleAddNode = modifyGraph defInverse action replyResult where
    action (AddNode.Request location nl@(NodeLoc _ nodeId) expression nodeMeta connectTo) = withDefaultResult location $ do
        Graph.addNodeWithConnection location nl expression nodeMeta connectTo

handleAddPort :: Request AddPort.Request -> StateT Env BusT ()
handleAddPort = modifyGraph defInverse action replyResult where
    action (AddPort.Request location portRef connsDst name) = withDefaultResult location $
        Graph.addPortWithConnections location portRef name connsDst

handleAddSubgraph :: Request AddSubgraph.Request -> StateT Env BusT ()
handleAddSubgraph = modifyGraph defInverse action replyResult where
    action (AddSubgraph.Request location nodes connections) = withDefaultResult location $
        Graph.addSubgraph location nodes connections

handleAutolayoutNodes :: Request AutolayoutNodes.Request -> StateT Env BusT ()
handleAutolayoutNodes = modifyGraph inverse action replyResult where
    inverse (AutolayoutNodes.Request location nodeLocs _) = do
        positions <- Graph.getNodeMetas location nodeLocs
        return $ AutolayoutNodes.Inverse $ catMaybes positions
    action (AutolayoutNodes.Request location nodeLocs _) = withDefaultResult location $
        Graph.autolayoutNodes location (convert <$> nodeLocs) --TODO[PM -> MM] Use NodeLoc instead of NodeId

handleCollapseToFunction :: Request CollapseToFunction.Request -> StateT Env BusT ()
handleCollapseToFunction = modifyGraph inverse action replyResult where
    inverse (CollapseToFunction.Request location@(GraphLocation file _) _) = do
        code <- Graph.withUnit (GraphLocation file def) $ use Graph.code
        cache <- Graph.prepareNodeCache (GraphLocation file def)
        return $ CollapseToFunction.Inverse code cache
    action (CollapseToFunction.Request location locs) = withDefaultResult location $ do
        let ids = convert <$> locs
        Graph.collapseToFunction location ids

handleCopy :: Request Copy.Request -> StateT Env BusT ()
handleCopy = modifyGraph defInverse action replyResult where
    action (Copy.Request location nodeLocs) = do
        r <- Graph.prepareCopy location (convert nodeLocs)
        return $ Copy.Result r r --FIXME

handleDumpGraphViz :: Request DumpGraphViz.Request -> StateT Env BusT ()
handleDumpGraphViz = modifyGraphOk defInverse action where
    action (DumpGraphViz.Request location) = Graph.dumpGraphViz location

handleGetSubgraphs :: Request GetSubgraphs.Request -> StateT Env BusT ()
handleGetSubgraphs = modifyGraph defInverse action replyResult where
    action (GetSubgraphs.Request location) = do
        graph <- Graph.getGraph location
        return $ GetSubgraphs.Result $ Map.singleton (location ^. GraphLocation.breadcrumb . Breadcrumb.items . to unsafeLast) graph --FIXME: should return multiple graphs

handleMovePort :: Request MovePort.Request -> StateT Env BusT ()
handleMovePort = modifyGraph defInverse action replyResult where
    action (MovePort.Request location portRef newPortPos) = withDefaultResult location $
        Graph.movePort location portRef newPortPos

handlePaste :: Request Paste.Request -> StateT Env BusT ()
handlePaste = modifyGraph defInverse action replyResult where
    action (Paste.Request location position string) = withDefaultResult location $ do
        Graph.paste location position string

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
        Graph allNodes allConnections _ _ monads <- Graph.getGraph location
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
        connections <- Graph.withGraph location $ runASTOp buildConnections
        oldName     <- Graph.getPortName location portRef
        let conns = flip filter connections $ (== portRef) . fst
        return $ RemovePort.Inverse oldName $ map (uncurry Connection) conns
    action (RemovePort.Request location portRef) = withDefaultResult location $ do
        maySidebar <- view GraphAPI.inputSidebar <$> Graph.getGraphNoTC location
        when (isNothing maySidebar) $ throwM SidebarDoesNotExistException
        Graph.removePort location portRef

handleRenameNode :: Request RenameNode.Request -> StateT Env BusT ()
handleRenameNode = modifyGraph inverse action replyResult where
    inverse (RenameNode.Request location nodeId name) = do
        prevName <- Graph.getName location nodeId
        return $ RenameNode.Inverse $ fromMaybe "" prevName
    action (RenameNode.Request location nodeId name) = withDefaultResult location $
        Graph.renameNode location nodeId name

handleRenamePort :: Request RenamePort.Request -> StateT Env BusT ()
handleRenamePort = modifyGraph inverse action replyResult where
    inverse (RenamePort.Request location portRef name) = do
        oldName <- Graph.getPortName location portRef
        return $ RenamePort.Inverse oldName
    action (RenamePort.Request location portRef name) = withDefaultResult location $
        Graph.renamePort location portRef name

handleSaveSettings :: Request SaveSettings.Request -> StateT Env BusT ()
handleSaveSettings = modifyGraphOk defInverse action where
    action (SaveSettings.Request gl settings) = saveSettings gl settings gl

handleSearchNodes :: Request SearchNodes.Request -> StateT Env BusT ()
handleSearchNodes origReq@(Request uuid guiID request'@(SearchNodes.Request location importsList)) = do
    request          <- liftIO $ webGUIHack request'
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    endPoints        <- use $ Env.config . to EP.clientFromConfig
    env              <- get
    toBusChan        <- use Env.toBusChan
    let invStatus = Response.Ok ()
    liftIO $ void $ forkIO $ void $ liftIO $ do
        result <- try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ SearchNodes.Result <$> Graph.getImports location importsList
        case result of
            Left  (exc :: SomeException) -> do
                err <- liftIO $ Graph.prepareLunaError exc
                let msg = Response.error origReq invStatus err
                atomically $ writeTChan toBusChan $ Message.Message (Topic.topic msg) $ Compress.pack $ Bin.encode msg
            Right (result, _) -> do
                let msg = Response.result origReq () result
                atomically $ writeTChan toBusChan $ Message.Message (Topic.topic msg) $ Compress.pack $ Bin.encode msg

handleSetCode :: Request SetCode.Request -> StateT Env BusT ()
handleSetCode = modifyGraph inverse action replyResult where
    inverse (SetCode.Request location@(GraphLocation file _) _ _) = do
        cache <- Graph.prepareNodeCache location
        code <- Graph.withUnit (GraphLocation file def) $ use Graph.code
        return $ SetCode.Inverse code cache
    action (SetCode.Request location@(GraphLocation file _) code cache) = withDefaultResultTC location $ do
        Graph.withUnit (GraphLocation file def) $ Graph.nodeCache .= cache
        Graph.loadCode location code
        Graph.resendCode location

handleSetNodeExpression :: Request SetNodeExpression.Request -> StateT Env BusT ()-- fixme [SB] returns Result with no new informations and change node expression has addNode+removeNodes
handleSetNodeExpression = modifyGraph inverse action replyResult where
    inverse (SetNodeExpression.Request location nodeId _) = do
        oldExpr <- Graph.withGraph location $ runASTOp $ GraphUtils.getASTTarget nodeId >>= Print.printExpression
        return $ SetNodeExpression.Inverse (Text.pack oldExpr)
    action (SetNodeExpression.Request location nodeId expression) = withDefaultResultTC location $
        Graph.setNodeExpression location nodeId expression

inverseSetNodesMeta :: GraphLocation -> [(NodeId, NodeMeta)] -> Empire SetNodesMeta.Inverse
inverseSetNodesMeta location updates = do
    allNodes <- Graph.withGraph' location (runASTOp buildNodes) (view GraphAPI.nodes <$> runASTOp buildClassGraph)
    let idSet = Set.fromList $ map fst updates
        prevMeta = catMaybes $ flip map allNodes $ \node ->
            if Set.member (node ^. Node.nodeId) idSet then
                 Just (node ^. Node.nodeId, node ^. Node.nodeMeta)
            else Nothing
    return $ SetNodesMeta.Inverse prevMeta

actionSetNodesMeta :: GraphLocation -> [(NodeId, NodeMeta)] -> Empire Result.Result
actionSetNodesMeta location updates = withDefaultResult location $
    for_ updates $ uncurry $ Graph.setNodeMeta location

handleSetNodesMeta :: Request SetNodesMeta.Request -> StateT Env BusT ()
handleSetNodesMeta = modifyGraph inverse action replyResult where
    inverse (SetNodesMeta.Request location updates) = inverseSetNodesMeta location updates
    action (SetNodesMeta.Request location updates) = actionSetNodesMeta location updates

handleSetNodesMetaUpdate :: SetNodesMeta.Update -> StateT Env BusT ()
handleSetNodesMetaUpdate (SetNodesMeta.Update location updates) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ actionSetNodesMeta location updates
    case result of
        Left  (exc :: SomeASTException) -> do
            err <- liftIO $ prettyException exc
            logger Logger.error err
        Right (result, newEmpireEnv) -> do
            Env.empireEnv .= newEmpireEnv

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
        Left (exc :: SomeASTException) -> do
            err <- liftIO $ Graph.prepareLunaError $ toException exc
            replyFail logger err req (Response.Error err)
        Right (_, newEmpireEnv) -> Env.empireEnv .= newEmpireEnv
    return ()

instance G.GraphRequest GetBuffer.Request where
    location = lens getter setter where
        getter (GetBuffer.Request file) = GraphLocation.GraphLocation file (Breadcrumb [])
        setter (GetBuffer.Request _   ) (GraphLocation.GraphLocation file _) = GetBuffer.Request file

handleSubstitute :: Request Substitute.Request -> StateT Env BusT ()
handleSubstitute = modifyGraph defInverse action replyResult where
    action req@(Substitute.Request location diffs) = do
        let file = location ^. GraphLocation.filePath
        prevImports <- Graph.getAvailableImports location
        res         <- withDefaultResult location $ Graph.substituteCodeFromPoints file diffs
        newImports  <- Graph.getAvailableImports location
        let importChange = if Set.fromList prevImports == Set.fromList newImports then Nothing else Just newImports
        if isJust importChange then do
            Graph.typecheckWithRecompute location
        else do
            Graph.withTC location False (return ())
        return $ Substitute.Result res importChange


handleGetBuffer :: Request GetBuffer.Request -> StateT Env BusT ()
handleGetBuffer = modifyGraph defInverse action replyResult where
    action (GetBuffer.Request file) = do
        code <- Graph.getBuffer file
        return $ GetBuffer.Result code

handleInterpreterControl :: Request Interpreter.Request -> StateT Env BusT ()
handleInterpreterControl = modifyGraph defInverse action replyResult where
    action request = Graph.setInterpreterState request

stdlibFunctions :: [String]
stdlibFunctions = ["mockFunction"]

stdlibMethods :: [String]
stdlibMethods = ["mockMethod"]
