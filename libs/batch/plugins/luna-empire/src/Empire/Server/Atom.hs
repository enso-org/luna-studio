{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Server.Atom where

import           Control.Exception.Safe         (try, catchAny)
import           Control.Lens                   ((.=), use)
import qualified Control.Monad.Catch            as MC
import           Control.Monad.State            (StateT)
import           Data.List                      (stripPrefix)
import qualified Data.Map                       as Map
import qualified Data.Text.IO                   as Text
import qualified Path
import           Prologue                       hiding (Item)
import qualified Shelly
import qualified System.Directory               as Dir
import           System.FilePath                (addTrailingPathSeparator, (</>))
import qualified System.IO                      as IO
import qualified System.IO.Temp                 as Temp

import           Empire.Env                     (Env)
import qualified Empire.Env                     as Env

import qualified LunaStudio.API.Atom.CloseFile  as CloseFile
import qualified LunaStudio.API.Atom.Copy       as Copy
import qualified LunaStudio.API.Atom.CreateProject as CreateProject
import qualified LunaStudio.API.Atom.IsSaved    as IsSaved
import qualified LunaStudio.API.Atom.OpenFile   as OpenFile
import qualified LunaStudio.API.Atom.Paste      as Paste
import qualified LunaStudio.API.Atom.SaveFile   as SaveFile
import qualified LunaStudio.API.Atom.MoveProject as MoveProject
import qualified LunaStudio.API.Atom.SetProject as SetProject
import qualified LunaStudio.API.Graph.Request   as G
import           LunaStudio.API.Request         (Request (..))
import qualified LunaStudio.API.Response        as Response
import           LunaStudio.Data.Breadcrumb     (Breadcrumb (..))
import qualified LunaStudio.Data.Error          as Error
import           LunaStudio.Data.GraphLocation  (GraphLocation(..))
import qualified LunaStudio.Data.GraphLocation  as GraphLocation
import qualified Luna.Package.Structure.Generate as PackageGen

import           Debug
import qualified Empire.Commands.Graph          as Graph
import qualified Empire.Commands.Package        as Package
import qualified Empire.Commands.Publisher      as Publisher
import           Empire.Data.AST                (SomeASTException)
import qualified Empire.Data.Graph              as Graph
import qualified Empire.Data.Library            as Library
import           Empire.Empire                  (Empire)
import qualified Empire.Empire                  as Empire
import           Empire.Server.Server           (errorMessage, defInverse, modifyGraph, replyFail,
                                                replyOk, replyResult, withDefaultResult)
import qualified System.Log.MLogger             as Logger
import qualified ZMQ.Bus.EndPoint               as EP
import           ZMQ.Bus.Trans                  (BusT (..))

import Debug.Trace (trace)

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

handleSetProject :: Request SetProject.Request -> StateT Env BusT ()
handleSetProject req = do
    liftIO $ putStrLn $ "OPEN PROJECT" <> show req
    replyOk req ()

replaceDir :: FilePath -> FilePath -> FilePath -> FilePath
replaceDir oldPath newPath path = case stripPrefix (addTrailingPathSeparator oldPath) path of
    Just suffix -> newPath </> suffix
    _           -> path

handleCreateProject :: Request CreateProject.Request -> StateT Env BusT ()
handleCreateProject req@(Request _ _ (CreateProject.Request path)) = do
    result <- PackageGen.genPackageStructure path Nothing def
    logger Logger.info $ show ("CREATE PROJECT", req, result)
    case result of
        Left generatorError -> do
            let lunaError = Package.prepareLunaError generatorError
            replyFail logger lunaError req (Response.Error lunaError)
        Right path' -> do
            replyOk req ()

handleMoveProject :: Request MoveProject.Request -> StateT Env BusT ()
handleMoveProject req@(Request _ _ (MoveProject.Request oldPath newPath)) = do
    result <- liftIO $ do
        r <- try $ Shelly.shelly $ do
            let conv = Shelly.fromText . convert
            Shelly.mkdir_p (conv newPath)
            Shelly.ls (conv oldPath) >>= mapM (flip Shelly.cp_r (conv newPath))
        Dir.removeDirectoryRecursive oldPath `catchAny` (\e -> logger Logger.error $ "couldn't remove directory" <> oldPath)
        return r
    case result of
        Left (e :: SomeException) -> do
            err <- liftIO $ Graph.prepareLunaError e
            replyFail logger err req (Response.Error err)
        Right _ -> do
            activeFiles <- use $ Env.empireEnv . Graph.userState . Empire.activeFiles
            let activeFilesList = Map.toList activeFiles
                changePath = replaceDir oldPath newPath
                newFiles = map (\(k, v) -> (changePath k, v & Library.path %~ changePath)) activeFilesList
            Env.empireEnv . Graph.userState . Empire.activeFiles .= Map.fromList newFiles
            replyOk req ()

handleOpenFile :: Request OpenFile.Request -> StateT Env BusT ()
handleOpenFile req@(Request _ _ (OpenFile.Request path)) = timeIt "handleOpenFile" $ do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.openFile path
    case result of
        Left (exc :: SomeASTException) -> do
            err <- liftIO $ Graph.prepareLunaError $ toException exc
            replyFail logger err req (Response.Error err)
        Right (_, newEmpireEnv)  -> do
            Env.empireEnv .= newEmpireEnv
            replyOk req ()

withClosedTempFile :: (MonadIO m, MC.MonadMask m) => FilePath -> String -> (FilePath -> m a) -> m a
withClosedTempFile dir template action = MC.bracket (liftIO mkFile)
                                                    (\name -> liftIO $ MC.catch (Dir.removeFile name) (\(e :: IOError) -> return ()))
                                                    action
   where
       mkFile = MC.bracket (IO.openTempFile dir template)
                           (IO.hClose . snd)
                           (return . fst)

handleSaveFile :: Request SaveFile.Request -> StateT Env BusT ()
handleSaveFile req@(Request _ _ (SaveFile.Request inPath)) = do
    currentEmpireEnv <- use Env.empireEnv
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
            path <- Path.parseAbsFile inPath
            let dir  = Path.toFilePath $ Path.parent path
                file = Path.toFilePath $ Path.filename path
            liftIO $ withClosedTempFile dir (file <> ".tmp") $ \tmpFile -> do
                Text.writeFile tmpFile source
                let backupFile = Path.toFilePath path <> ".backup"
                Dir.renameFile (Path.toFilePath path) backupFile
                Dir.renameFile tmpFile (Path.toFilePath path)
                Dir.removeFile backupFile
            replyOk req ()

handleCloseFile :: Request CloseFile.Request -> StateT Env BusT ()
handleCloseFile (Request _ _ (CloseFile.Request path)) = do
    Env.empireEnv . Graph.userState . Empire.activeFiles . at path .= Nothing
    empireNotifEnv   <- use Env.empireNotif
    currentEmpireEnv <- use Env.empireEnv
    void $ liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Publisher.stopTC

handleIsSaved :: Request IsSaved.Request -> StateT Env BusT ()
handleIsSaved (Request _ _ _) = $_NOT_IMPLEMENTED

handlePasteText :: Request Paste.Request -> StateT Env BusT ()
handlePasteText = modifyGraph defInverse action replyResult where
    action (Paste.Request loc spans text) = withDefaultResult loc $ do
        Graph.pasteText loc spans text

instance G.GraphRequest Copy.Request where
    location = lens getter setter where
        getter (Copy.Request file _) = GraphLocation.GraphLocation file (Breadcrumb [])
        setter (Copy.Request _ spans) (GraphLocation.GraphLocation file _) = Copy.Request file spans

handleCopyText :: Request Copy.Request -> StateT Env BusT ()
handleCopyText = modifyGraph defInverse action replyResult where
    action (Copy.Request path spans) = do
        Copy.Result <$> Graph.copyText (GraphLocation path (Breadcrumb [])) spans
