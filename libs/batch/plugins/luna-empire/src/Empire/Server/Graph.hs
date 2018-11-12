module Empire.Server.Graph where

import Prologue hiding (Item, when)

import qualified Compress
import qualified Data.Binary                             as Bin
import qualified Data.HashMap.Strict                     as HashMap
import qualified Data.IntMap                             as IntMap
import qualified Data.Map                                as Map
import qualified Data.Set                                as Set
import qualified Data.Text                               as Text
import qualified Data.UUID.Types                         as UUID
import qualified Data.UUID.V4                            as UUID
import qualified Empire.ApiHandlers                      as Api
import qualified Empire.ASTOps.Print                     as Print
import qualified Empire.Commands.Graph                   as Graph
import qualified Empire.Commands.GraphBuilder            as GraphBuilder
import qualified Empire.Data.Graph                       as Graph (code,
                                                                   nodeCache)
import qualified Empire.Empire                           as Empire
import qualified Empire.Env                              as Env
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
import qualified LunaStudio.API.Graph.SaveSettings       as SaveSettings
import qualified LunaStudio.API.Graph.SearchNodes        as SearchNodes
import qualified LunaStudio.API.Graph.SetCode            as SetCode
import qualified LunaStudio.API.Graph.SetNodeExpression  as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta       as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault     as SetPortDefault
import qualified LunaStudio.API.Graph.TypeCheck          as TypeCheck
import qualified LunaStudio.API.Request                  as Request
import qualified LunaStudio.API.Response                 as Response
import qualified LunaStudio.API.Topic                    as Topic
import qualified LunaStudio.Data.Breadcrumb              as Breadcrumb
import qualified LunaStudio.Data.CameraTransformation    as Camera
import qualified LunaStudio.Data.Diff                    as Diff
import qualified LunaStudio.Data.Graph                   as GraphAPI
import qualified LunaStudio.Data.GraphLocation           as GraphLocation
import qualified LunaStudio.Data.GUIState                as GUIState
import qualified LunaStudio.Data.Node                    as Node
import qualified LunaStudio.Data.NodeLoc                 as NodeLoc
import qualified LunaStudio.Data.NodeMeta                as NodeMeta
import qualified LunaStudio.Data.NodeSearcher            as NS
import qualified LunaStudio.Data.Port                    as Port
import qualified LunaStudio.Data.Position                as Position
import qualified LunaStudio.Data.Project                 as Project
import qualified Path
import qualified Safe
import qualified System.Log.MLogger                      as Logger
import qualified ZMQ.Bus.Bus                             as Bus
import qualified ZMQ.Bus.Config                          as Config
import qualified ZMQ.Bus.Data.Message                    as Message
import qualified ZMQ.Bus.EndPoint                        as EP
import qualified ZMQ.Bus.Trans                           as BusT

import Control.Arrow                 ((&&&))
import Control.Concurrent            (forkIO)
import Control.Concurrent.MVar       (readMVar)
import Control.Concurrent.STM.TChan  (writeTChan)
import Control.Error                 (runExceptT)
import Control.Lens                  (to, traversed, use, (.=), (^..))
import Control.Monad                 (when)
import Control.Monad.Catch           (handle, try)
import Control.Monad.Reader          (asks)
import Control.Monad.State           (StateT, evalStateT, get)
import Control.Monad.STM             (atomically)
import Data.ByteString               (ByteString)
import Data.ByteString.Lazy          (fromStrict)
import Data.Char                     (isUpper)
import Data.List                     (break, find, partition, sortBy)
import Data.List.Split               (splitOneOf)
import Data.Map                      (Map)
import Data.Maybe                    (isJust, isNothing, listToMaybe,
                                      maybeToList)
import Data.Text                     (stripPrefix)
import Data.Traversable              (forM)
import Data.UUID.Types               (UUID)
import Empire.ASTOp                  (runASTOp)
import Empire.Commands.Autolayout    (autolayoutNodes)
import Empire.Commands.GraphBuilder  (buildClassGraph, buildConnections,
                                      buildGraph, buildNodes, getNodeCode,
                                      getNodeName)
import Empire.Data.AST               (SomeASTException,
                                      astExceptionFromException,
                                      astExceptionToException)
import Empire.Empire                 (Empire)
import Empire.Env                    (Env)
import Empire.Server.Server          (defInverse, errorMessage, modifyGraph,
                                      modifyGraphOk, prettyException, replyFail,
                                      replyOk, replyResult, sendToBus',
                                      webGUIHack, withDefaultResult,
                                      withDefaultResultTC)
import Luna.Package                  (findPackageFileForFile,
                                      findPackageRootForFile,
                                      getRelativePathForModule, includedLibs)
import LunaStudio.API.Request        (Request (..))
import LunaStudio.Data.Breadcrumb    (Breadcrumb (..))
import LunaStudio.Data.Code          (Code (Code))
import LunaStudio.Data.Connection    as Connection
import LunaStudio.Data.Diff          (Diff, diff, guiStateDiff)
import LunaStudio.Data.Graph         (Graph (..))
import LunaStudio.Data.GraphLocation (GraphLocation (..))
import LunaStudio.Data.GUIState      (GUIState (GUIState))
import LunaStudio.Data.LabeledTree   (LabeledTree (LabeledTree))
import LunaStudio.Data.Node          (ExpressionNode (..), NodeId)
import LunaStudio.Data.NodeLoc       (NodeLoc (..))
import LunaStudio.Data.NodeMeta      (NodeMeta)
import LunaStudio.Data.NodeValue     (NodeValue (NodeValue))
import LunaStudio.Data.Port          (InPort (..), InPortIndex (..),
                                      OutPort (..), OutPortIndex (..),
                                      Port (..), PortState (..), getPortNumber)
import LunaStudio.Data.PortDefault   (PortValue (..))
import LunaStudio.Data.PortRef       (InPortRef (..), OutPortRef (..))
import LunaStudio.Data.PortRef       as PortRef
import LunaStudio.Data.Position      (Position)
import LunaStudio.Data.Project       (LocationSettings)
import LunaStudio.Data.TypeRep       (TypeRep (TStar))
import LunaStudio.Data.Visualization (VisualizationValue (..))
import LunaStudio.Data.Visualizer    (ExternalVisualizers (ExternalVisualizers))
import Path                          (fromAbsFile, fromRelFile, parseAbsFile)
import System.Environment            (getEnv)
import System.FilePath               (dropFileName, replaceFileName, (</>))
import ZMQ.Bus.Trans                 (BusT (..))


logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

logProjectPathNotFound :: MonadIO m => m ()
logProjectPathNotFound
    = Project.logProjectSettingsError "Could not find project path."
-- helpers


generateNodeId :: IO NodeId
generateNodeId = UUID.nextRandom

getAllNodes :: GraphLocation -> Empire [Node.Node]
getAllNodes location = do
    graph <- Graph.getGraph location
    let inputSidebarList  = maybeToList $ graph ^. GraphAPI.inputSidebar
        outputSidebarList = maybeToList $ graph ^. GraphAPI.outputSidebar
    pure $ fmap Node.ExpressionNode' (graph ^. GraphAPI.nodes)
        <> fmap Node.InputSidebar'   inputSidebarList
        <> fmap Node.OutputSidebar'  outputSidebarList

getNodesByIds :: GraphLocation -> [NodeId] -> Empire [Node.Node]
getNodesByIds location nids = filterRelevantNodes <$> getAllNodes location where
    filterRelevantNodes
        = filter (flip Set.member requestedIDs . view Node.nodeId)
    requestedIDs = fromList nids

getExpressionNodesByIds :: GraphLocation -> [NodeId] -> Empire [ExpressionNode]
getExpressionNodesByIds location nids
    = filterRelevantNodes <$> Graph.getNodes location where
        filterRelevantNodes
            = filter (flip Set.member requestedIDs . view Node.nodeId)
        requestedIDs = fromList nids

getNodeById :: GraphLocation -> NodeId -> Empire (Maybe Node.Node)
getNodeById location nid
    = find (\n -> n ^. Node.nodeId == nid) <$> getAllNodes location

getProjectPathAndRelativeModulePath :: MonadIO m
    => FilePath -> m (Maybe (FilePath, FilePath))
getProjectPathAndRelativeModulePath modulePath = do
    let eitherToMaybe :: MonadIO m
            => Either Path.PathException (Path.Path Path.Abs Path.File)
            -> m (Maybe (Path.Path Path.Abs Path.File))
        eitherToMaybe (Left  e) = Project.logProjectSettingsError e >> pure def
        eitherToMaybe (Right a) = pure $ Just a
    mayProjectPathAndRelModulePath <- liftIO . runMaybeT $ do
        absModulePath  <- MaybeT $
            eitherToMaybe =<< try (parseAbsFile modulePath)
        absProjectPath <- MaybeT $ findPackageFileForFile absModulePath
        relModulePath  <- MaybeT $
            getRelativePathForModule absProjectPath absModulePath
        pure (fromAbsFile absProjectPath, fromRelFile relModulePath)
    when (isNothing mayProjectPathAndRelModulePath) logProjectPathNotFound
    pure mayProjectPathAndRelModulePath

saveSettings :: GraphLocation -> LocationSettings -> GraphLocation -> Empire ()
saveSettings gl settings newGl = handle logError action where
    logError :: MonadIO m => SomeException -> m ()
    logError e = Project.logProjectSettingsError e
    action     = do
        bc    <- Breadcrumb.toNames <$> Graph.decodeLocation gl
        newBc <- Breadcrumb.toNames <$> Graph.decodeLocation newGl
        let filePath        = gl    ^. GraphLocation.filePath
            newFilePath     = newGl ^. GraphLocation.filePath
            lastBcInOldFile = if filePath == newFilePath then newBc else bc
        withJustM (getProjectPathAndRelativeModulePath filePath) $ \(cp, fp) ->
            Project.updateLocationSettings cp fp bc settings lastBcInOldFile
        when (filePath /= newFilePath)
            $ withJustM (getProjectPathAndRelativeModulePath newFilePath)
                $ \(cp, fp) ->
                    Project.updateCurrentBreadcrumbSettings cp fp newBc

getClosestBcLocation :: GraphLocation -> Breadcrumb Text -> Empire GraphLocation
getClosestBcLocation gl (Breadcrumb []) = pure gl
getClosestBcLocation gl (Breadcrumb (nodeName:newBcItems)) = do
    g <- Graph.getGraph gl
    let mayN = find ((Just nodeName ==) . view Node.name) (g ^. GraphAPI.nodes)
        processLocation n = do
            let nid = n ^. Node.nodeId
                bci = if n ^. Node.isDefinition
                    then Breadcrumb.Definition nid
                    else Breadcrumb.Lambda nid
                nextLocation = gl & GraphLocation.breadcrumb . Breadcrumb.items
                    %~ (<>[bci])
            getClosestBcLocation nextLocation $ Breadcrumb newBcItems
    maybe (pure gl) processLocation mayN


-- Handlers


handleGetProgram :: Request GetProgram.Request -> StateT Env BusT ()
handleGetProgram = modifyGraph defInverse action replyResult where
    action (GetProgram.Request location' mayPrevSettings retrieveLocation) = do
        let moduleChanged = isNothing mayPrevSettings
                || isJust (join $ view Project.visMap . snd <$> mayPrevSettings)
            makeError :: MonadIO m
                => SomeASTException -> m (GraphLocation, GUIState)
            makeError e = pure $ (location', GUIState
                (Breadcrumb [])
                mempty
                def
                def
                mempty
                . Left . Graph.prepareGraphError $ toException e)
        (location, guiState) <- handle makeError $ do
            let filePath      = location' ^. GraphLocation.filePath
                closestBc loc = getClosestBcLocation
                    (GraphLocation.GraphLocation filePath def)
            mayProjectPathAndRelModulePath <- liftIO
                $ getProjectPathAndRelativeModulePath filePath
            mayPackageRoot <- findPackageRootForFile
                =<< Path.parseAbsFile filePath
            mayModuleSettings <- liftIO $ maybe
                (pure def)
                (uncurry Project.getModuleSettings)
                mayProjectPathAndRelModulePath
            location <- if not retrieveLocation
                then pure location'
                else getClosestBcLocation
                    (GraphLocation.GraphLocation filePath def)
                    $ maybe
                        (Breadcrumb ["main"])
                        (view Project.currentBreadcrumb)
                        mayModuleSettings
            graph <- Graph.getGraph location
            crumb <- Graph.decodeLocation location
            code  <- Code <$> Graph.getCode location
            libsVisPaths <- Map.fromList . fmap
                (\(libName, visLibPath)
                    -> (convert libName, visLibPath </> "visualizers"))
                <$> includedLibs
            let mayProjectVisPath
                    = ((</> "visualizers") . Path.toFilePath) <$> mayPackageRoot
                externalVisPaths
                    = ExternalVisualizers mayProjectVisPath libsVisPaths
                defaultCamera = maybe
                    def
                    (`Camera.getCameraForRectangle` def)
                    . Position.minimumRectangle
                        $ graph ^.. GraphAPI.nodes . traversed . Node.position
                (typeRepToVisMap, camera) = case mayModuleSettings of
                    Nothing -> (mempty, defaultCamera)
                    Just ms ->
                        let visMap = Project.fromOldAPI
                                <$> ms ^. Project.typeRepToVisMap
                            bc = Breadcrumb.toNames crumb
                            bs = Map.lookup bc
                                $ ms ^. Project.breadcrumbsSettings
                            cam = maybe
                                defaultCamera
                                (view Project.breadcrumbCameraSettings)
                                bs
                        in (visMap, cam)
            pure $ (location, GUIState
                crumb
                typeRepToVisMap
                camera
                externalVisPaths
                code
                $ Right graph)
        withJust mayPrevSettings
            $ \(gl, locSettings) -> saveSettings gl locSettings location
        pure . GetProgram.Result location $ guiStateDiff guiState

handleAddConnection :: Request AddConnection.Request -> StateT Env BusT ()
handleAddConnection req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleAddImports :: Request AddImports.Request -> StateT Env BusT ()
handleAddImports req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleAddNode :: Request AddNode.Request -> StateT Env BusT ()
handleAddNode req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleAddPort :: Request AddPort.Request -> StateT Env BusT ()
handleAddPort req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleAddSubgraph :: Request AddSubgraph.Request -> StateT Env BusT ()
handleAddSubgraph req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleAutolayoutNodes :: Request AutolayoutNodes.Request -> StateT Env BusT ()
handleAutolayoutNodes req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleCollapseToFunction :: Request CollapseToFunction.Request -> StateT Env BusT ()
handleCollapseToFunction req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleCopy :: Request Copy.Request -> StateT Env BusT ()
handleCopy = modifyGraph Api.buildInverse Api.perform replyResult

handleDumpGraphViz :: Request DumpGraphViz.Request -> StateT Env BusT ()
handleDumpGraphViz = modifyGraphOk Api.buildInverse Api.perform

handleGetSubgraphs :: Request GetSubgraphs.Request -> StateT Env BusT ()
handleGetSubgraphs = modifyGraph Api.buildInverse Api.perform replyResult

handleMovePort :: Request MovePort.Request -> StateT Env BusT ()
handleMovePort req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handlePaste :: Request Paste.Request -> StateT Env BusT ()
handlePaste req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleRemoveConnection :: Request RemoveConnection.Request -> StateT Env BusT ()
handleRemoveConnection req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleRemoveNodes :: Request RemoveNodes.Request -> StateT Env BusT ()
handleRemoveNodes req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleRemovePort :: Request RemovePort.Request -> StateT Env BusT ()
handleRemovePort req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleRenameNode :: Request RenameNode.Request -> StateT Env BusT ()
handleRenameNode req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleRenamePort :: Request RenamePort.Request -> StateT Env BusT ()
handleRenamePort req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleSaveSettings :: Request SaveSettings.Request -> StateT Env BusT ()
handleSaveSettings = modifyGraphOk defInverse action where
    action (SaveSettings.Request gl settings) = saveSettings gl settings gl

handleSearchNodes :: Request SearchNodes.Request -> StateT Env BusT ()
handleSearchNodes origReq@(Request uuid guiID
    request'@(SearchNodes.Request location missingImps)) = do
        request          <- liftIO $ webGUIHack request'
        currentEmpireEnv <- use Env.empireEnv
        empireNotifEnv   <- use Env.empireNotif
        endPoints        <- use $ Env.config . to EP.clientFromConfig
        env              <- get
        toBusChan        <- use Env.toBusChan
        let invStatus = Response.Ok ()
        liftIO $ void $ forkIO $ void $ liftIO $ do
            result <- try . Empire.runEmpire empireNotifEnv currentEmpireEnv $ do
                Graph.addImports location missingImps
                SearchNodes.Result <$> Graph.getSearcherHints location
            case result of
                Left (exc :: SomeException) -> do
                    err <- liftIO $ Graph.prepareLunaError exc
                    let msg = Response.error origReq invStatus err
                    atomically . writeTChan toBusChan
                        . Message.Message (Topic.topic msg)
                            . Compress.pack $ Bin.encode msg
                Right (result, _) -> do
                    let msg = Response.result origReq () result
                    atomically . writeTChan toBusChan
                        . Message.Message (Topic.topic msg)
                            . Compress.pack $ Bin.encode msg

handleSetCode :: Request SetCode.Request -> StateT Env BusT ()
handleSetCode req = modifyGraph Api.buildInverse
    (withDefaultResultTC (req ^. G.location) . Api.perform) replyResult req

handleSetNodeExpression :: Request SetNodeExpression.Request -> StateT Env BusT ()
handleSetNodeExpression = modifyGraph inverse action replyResult where
    inverse (SetNodeExpression.Request location nodeId _) = do
        oldExpr <- Graph.withGraph location . runASTOp $
            GraphBuilder.getNodeCode nodeId
        pure $ SetNodeExpression.Inverse oldExpr
    action (SetNodeExpression.Request location nodeId expression)
        = withDefaultResultTC location $
            Graph.setNodeExpression location nodeId expression

inverseSetNodesMeta :: GraphLocation -> Map NodeId NodeMeta
    -> Empire SetNodesMeta.Inverse
inverseSetNodesMeta location updates = do
    allNodes <- Graph.withBreadcrumb location (runASTOp buildNodes)
        $ view GraphAPI.nodes <$> runASTOp buildClassGraph
    let prevMeta = Map.fromList . catMaybes . flip fmap allNodes $ \node ->
            justIf
                (Map.member (node ^. Node.nodeId) updates)
                (node ^. Node.nodeId, node ^. Node.nodeMeta)
    pure $ SetNodesMeta.Inverse prevMeta

actionSetNodesMeta :: GraphLocation -> Map NodeId NodeMeta -> Empire Diff
actionSetNodesMeta location updates = withDefaultResult location $
    for_ (toList updates) $ uncurry $ Graph.setNodeMeta location

handleSetNodesMeta :: Request SetNodesMeta.Request -> StateT Env BusT ()
handleSetNodesMeta = modifyGraph inverse action replyResult where
    inverse (SetNodesMeta.Request location updates)
        = inverseSetNodesMeta location updates
    action  (SetNodesMeta.Request location updates)
        = actionSetNodesMeta location updates

handleSetNodesMetaUpdate :: SetNodesMeta.Update -> StateT Env BusT ()
handleSetNodesMetaUpdate (SetNodesMeta.Update location updates) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result <- liftIO . try . Empire.runEmpire empireNotifEnv currentEmpireEnv
        $ actionSetNodesMeta location updates
    case result of
        Left (exc :: SomeASTException) -> do
            err <- liftIO $ prettyException exc
            logger Logger.error err
        Right (result, newEmpireEnv) -> Env.empireEnv .= newEmpireEnv

handleSetPortDefault :: Request SetPortDefault.Request -> StateT Env BusT ()
handleSetPortDefault req = modifyGraph Api.buildInverse
    (withDefaultResult (req ^. G.location) . Api.perform) replyResult req

handleTypecheck :: Request TypeCheck.Request -> StateT Env BusT ()
handleTypecheck req@(Request _ _ request) = do
    let location = request ^. TypeCheck.location
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result           <- liftIO . try
        . Empire.runEmpire empireNotifEnv currentEmpireEnv
            $ Graph.typecheck location
    case result of
        Left (exc :: SomeASTException) -> do
            err <- liftIO $ Graph.prepareLunaError $ toException exc
            replyFail logger err req (Response.Error err)
        Right (_, newEmpireEnv) -> Env.empireEnv .= newEmpireEnv
    pure ()

instance G.GraphRequest GetBuffer.Request where
    location = lens getter setter where
        getter (GetBuffer.Request file)
            = GraphLocation.GraphLocation file (Breadcrumb [])
        setter (GetBuffer.Request _   ) (GraphLocation.GraphLocation file _)
            = GetBuffer.Request file

handleSubstitute :: Request Substitute.Request -> StateT Env BusT ()
handleSubstitute = modifyGraph defInverse action replyResult where
    action req@(Substitute.Request location diffs) = do
        let file = location ^. GraphLocation.filePath
        graphDiff <- withDefaultResult location
            $ Graph.substituteCodeFromPoints file diffs
        Graph.typecheckWithRecompute location
        pure graphDiff


handleGetBuffer :: Request GetBuffer.Request -> StateT Env BusT ()
handleGetBuffer = modifyGraph defInverse action replyResult where
    action (GetBuffer.Request file) = do
        code <- Graph.getBuffer file
        pure $ GetBuffer.Result code

handleInterpreterControl :: Request Interpreter.Request -> StateT Env BusT ()
handleInterpreterControl = modifyGraph defInverse action replyResult where
    interpreterAction Interpreter.Start  = Graph.startInterpreter
    interpreterAction Interpreter.Pause  = Graph.pauseInterpreter
    interpreterAction Interpreter.Reload = Graph.reloadInterpreter
    action (Interpreter.Request gl command) = interpreterAction command gl

stdlibFunctions :: [String]
stdlibFunctions = ["mockFunction"]

stdlibMethods :: [String]
stdlibMethods = ["mockMethod"]
