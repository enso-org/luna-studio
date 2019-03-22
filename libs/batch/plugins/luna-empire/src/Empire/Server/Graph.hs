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
import qualified Empire.Server.Server                    as Server
import qualified Luna.Datafile.Stdlib                    as StdLocator
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
                                      modifyGraphOk, replyFail,
                                      replyOk, replyResult, sendToBus')
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


-- Handlers


handleGetProgram :: Request GetProgram.Request -> StateT Env BusT ()
handleGetProgram = modifyGraph defInverse action replyResult where
    action (GetProgram.Request location' mayPrevSettings retrieveLocation) = do
        let moduleChanged = isNothing mayPrevSettings
                || isJust (join $ view Project.visMap . snd <$> mayPrevSettings)
            makeError :: MonadIO m
                => SomeASTException -> m (GraphLocation, GUIState)
            makeError e = liftIO (Graph.prepareGraphError (toException e)) >>= \err -> pure (location', GUIState
                (Breadcrumb [])
                mempty
                def
                def
                mempty
                (Left err))
        (location, guiState) <- handle makeError $ do
            let filePath      = location' ^. GraphLocation.filePath
                closestBc loc = Api.getClosestBcLocation
                    (GraphLocation.GraphLocation filePath def)
            mayProjectPathAndRelModulePath <- liftIO
                $ Api.getProjectPathAndRelativeModulePath filePath
            mayPackageRoot <- findPackageRootForFile
                =<< Path.parseAbsFile filePath
            mayModuleSettings <- liftIO $ maybe
                (pure def)
                (uncurry Project.getModuleSettings)
                mayProjectPathAndRelModulePath
            location <- if not retrieveLocation
                then pure location'
                else Api.getClosestBcLocation
                    (GraphLocation.GraphLocation filePath def)
                    $ maybe
                        (Breadcrumb ["main"])
                        (view Project.currentBreadcrumb)
                        mayModuleSettings
            graph <- Graph.getGraph location
            crumb <- Graph.decodeLocation location
            code  <- Code <$> Graph.getCode location
            stdPath <- StdLocator.findPath
            libsVisPaths <- Map.fromList . fmap
                (\(libName, visLibPath)
                    -> (convert libName, visLibPath </> "visualizers"))
                <$> includedLibs stdPath
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
            $ \(gl, locSettings) -> Api.saveSettings gl locSettings location
        pure . GetProgram.Result location $ guiStateDiff guiState

handleSearchNodes :: Request SearchNodes.Request -> StateT Env BusT ()
handleSearchNodes origReq@(Request uuid guiID
    request@(SearchNodes.Request location missingImps)) = do
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
                        . Message.Message (Topic.topic' msg)
                            . Compress.pack $ Bin.encode msg
                Right (result, _) -> do
                    let msg = Response.result origReq () result
                    atomically . writeTChan toBusChan
                        . Message.Message (Topic.topic' msg)
                            . Compress.pack $ Bin.encode msg

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
            replyFail Api.logger err req (Response.Error err)
        Right (_, newEmpireEnv) -> Env.empireEnv .= newEmpireEnv
    pure ()

stdlibFunctions :: [String]
stdlibFunctions = ["mockFunction"]

stdlibMethods :: [String]
stdlibMethods = ["mockMethod"]
