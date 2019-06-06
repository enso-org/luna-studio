{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Server.Atom where


import Prologue hiding (Item)

import qualified Bus.Framework.App                 as Bus
import qualified Control.Monad.Catch               as MC
import qualified Data.Text.IO                      as Text
import qualified Empire.ApiHandlers                as Api
import qualified Empire.Commands.Graph             as Graph
import qualified Empire.Commands.Package           as Package
import qualified Empire.Commands.Publisher         as Publisher
import qualified Empire.Data.Graph                 as Graph
import qualified Empire.Data.Library               as Library
import qualified Empire.Empire                     as Empire
import qualified Empire.Empire                     as Empire
import qualified Empire.Env                        as Env
import qualified Empire.Server.Server              as Server
import qualified Luna.Package                      as Package
import qualified Luna.Package.Structure.Generate   as PackageGen
import qualified LunaStudio.API.Atom.CloseFile     as CloseFile
import qualified LunaStudio.API.Atom.Copy          as Copy
import qualified LunaStudio.API.Atom.CreateProject as CreateProject
import qualified LunaStudio.API.Atom.IsSaved       as IsSaved
import qualified LunaStudio.API.Atom.MoveProject   as MoveProject
import qualified LunaStudio.API.Atom.OpenFile      as OpenFile
import qualified LunaStudio.API.Atom.Paste         as Paste
import qualified LunaStudio.API.Atom.SaveFile      as SaveFile
import qualified LunaStudio.API.Atom.SetProject    as SetProject
import qualified LunaStudio.API.Graph.Request      as G
import qualified LunaStudio.API.Response           as Response
import qualified LunaStudio.Data.Error             as Error
import qualified LunaStudio.Data.GraphLocation     as GraphLocation
import qualified Path
import qualified System.Directory                  as Dir
import qualified System.IO                         as IO
import qualified System.IO.Temp                    as Temp
import qualified System.Log.MLogger                as Logger

import Control.Exception.Safe        (catchAny, try)
import Control.Lens                  (use, (.=), _Just)
import Control.Monad.State           (StateT, forM)
import Data.List                     (stripPrefix, (++))
import Empire.Data.AST               (SomeASTException)
import Empire.Empire                 (Empire)
import Empire.Env                    (Env)
import Empire.Server.Server          (defInverse, errorMessage, modifyGraph,
                                      replyFail, replyOk, replyResult)
import LunaStudio.API.Request        (Request (..))
import LunaStudio.Data.Breadcrumb    (Breadcrumb (..))
import LunaStudio.Data.GraphLocation (GraphLocation (..))
import Path                          ((</>))

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

handleSetProject :: Request SetProject.Request -> StateT Env Bus.App ()
handleSetProject req@(Request _ _ (SetProject.Request path)) = do
    pmState <- liftIO Graph.defaultPMState
    let cmdState = Graph.CommandState pmState $ Empire.mkEnv path
    Env.empireEnv .= Just cmdState
    empireNotifEnv <- use Env.empireNotif
    liftIO $ Empire.runEmpire empireNotifEnv cmdState $
        Publisher.setProjectTC path
    replyOk req ()

handleCreateProject :: Request CreateProject.Request -> StateT Env Bus.App ()
handleCreateProject req@(Request _ _ (CreateProject.Request path)) = do
    pure ()

handleMoveProject :: Request MoveProject.Request -> StateT Env Bus.App ()
handleMoveProject req@(Request _ _ (MoveProject.Request oldPath newPath)) = do
    pure ()

handleOpenFile :: Request OpenFile.Request -> StateT Env Bus.App ()
handleOpenFile req@(Request _ _ (OpenFile.Request path)) = do
    currentEmpireEnv <- use Env.empireEnv
    case currentEmpireEnv of
        Just empireEnv -> do
            empireNotifEnv   <- use Env.empireNotif
            filePath <- Path.parseRelFile path
            result <- liftIO $ try $ Empire.runEmpire empireNotifEnv empireEnv $ Graph.openFile filePath
            case result of
                Left (exc :: SomeException) -> do
                    err <- liftIO $ Graph.prepareLunaError $ toException exc
                    replyFail logger err req (Response.Error err)
                Right (_, newEmpireEnv)  -> do
                    Env.empireEnv .= Just newEmpireEnv
                    replyOk req ()
        Nothing -> pure ()

withClosedTempFile :: (MonadIO m, MC.MonadMask m) => FilePath -> String -> (FilePath -> m a) -> m a
withClosedTempFile dir template action = MC.bracket (liftIO mkFile)
                                                    (\name -> liftIO $ MC.catch (Dir.removeFile name) (\(e :: IOError) -> return ()))
                                                    action
   where
       mkFile = MC.bracket (IO.openTempFile dir template)
                           (IO.hClose . snd)
                           (return . fst)

handleSaveFile :: Request SaveFile.Request -> StateT Env Bus.App ()
handleSaveFile req@(Request _ _ (SaveFile.Request inPath)) = do
    Server.withActiveProject $ \currentEmpireEnv -> do
        empireNotifEnv   <- use Env.empireNotif
        res <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ do
            (parseError, code) <- Graph.withUnit (GraphLocation inPath (Breadcrumb [])) $ do
                (,) <$> use (Graph.userState . Graph.clsParseError) <*> use Graph.code
            case parseError of
                Just _ -> return code
                _      -> Graph.addMetadataToCode inPath
        case res of
            Left (exc :: SomeASTException) -> do
                err <- liftIO $ Graph.prepareLunaError $ toException exc
                replyFail logger err req (Response.Error err)
            Right (source, _newEmpireEnv) -> do
                -- we ignore the resulting state so addMetadataToCode can't mess with our code in buffer
                -- only result is useful so it's ok
                let rootPath = currentEmpireEnv ^. Graph.userState . Empire.activeProject
                    path     = rootPath </> inPath
                let dir  = Path.toFilePath $ Path.parent path
                    file = Path.toFilePath $ Path.filename path
                liftIO $ withClosedTempFile dir (file <> ".tmp") $ \tmpFile -> do
                    Text.writeFile tmpFile source
                    Dir.renameFile tmpFile (Path.toFilePath path)
                replyOk req ()

handleCloseFile :: Request CloseFile.Request -> StateT Env Bus.App ()
handleCloseFile (Request _ _ (CloseFile.Request path)) = do
    Server.withActiveProject $ \currentEmpireEnv -> do
        Env.empireEnv . _Just . Graph.userState . Empire.activeFiles . at path .= Nothing
        empireNotifEnv <- use Env.empireNotif
        void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Publisher.stopTC

handleIsSaved :: Request IsSaved.Request -> StateT Env Bus.App ()
handleIsSaved (Request _ _ _) = $_NOT_IMPLEMENTED

handlePasteText :: Request Paste.Request -> StateT Env Bus.App ()
handlePasteText = modifyGraph defInverse action replyResult where
    action (Paste.Request loc spans text) = Api.withDiff loc $ do
        Graph.pasteText loc spans text

handleCopyText :: Request Copy.Request -> StateT Env Bus.App ()
handleCopyText = modifyGraph defInverse action replyResult where
    action (Copy.Request path spans) =
        Copy.Result <$> Graph.copyText (GraphLocation path (Breadcrumb [])) spans
