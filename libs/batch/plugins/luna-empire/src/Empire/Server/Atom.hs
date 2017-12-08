{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Server.Atom where

import           Control.Exception.Safe         (try, catchAny)
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
import           LunaStudio.Data.GraphLocation  (GraphLocation(..))
import qualified LunaStudio.Data.GraphLocation  as GraphLocation

import           Debug
import qualified Empire.Commands.Graph          as Graph
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
            let err = displayException e
            replyFail logger err req (Response.Error err)
        Right _ -> do
            activeFiles <- use $ Env.empireEnv . Empire.activeFiles
            let activeFilesList = Map.toList activeFiles
                changePath = replaceDir oldPath newPath
                newFiles = map (\(k, v) -> (changePath k, v & Library.path %~ changePath)) activeFilesList
            Env.empireEnv . Empire.activeFiles .= Map.fromList newFiles
            replyOk req ()

handleOpenFile :: Request OpenFile.Request -> StateT Env BusT ()
handleOpenFile req@(Request _ _ (OpenFile.Request path)) = timeIt "handleOpenFile" $ do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.openFile path
    case result of
        Left (exc :: SomeASTException) ->
            let err = displayException exc in replyFail logger err req (Response.Error err)
        Right (_, newEmpireEnv)  -> do
            Env.empireEnv .= newEmpireEnv
            replyOk req ()

handleSaveFile :: Request SaveFile.Request -> StateT Env BusT ()
handleSaveFile req@(Request _ _ (SaveFile.Request inPath)) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    res <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ do
        (parseError, code) <- Graph.withUnit (GraphLocation inPath (Breadcrumb [])) $ do
            (,) <$> use Graph.clsParseError <*> use Graph.code
        case parseError of
            Just _ -> return code
            _      -> Graph.addMetadataToCode inPath
    case res of
        Left (exc :: SomeASTException) ->
            let err = displayException exc in replyFail logger err req (Response.Error err)
        Right (source, _newEmpireEnv) -> do
            -- we ignore the resulting state so addMetadataToCode can't mess with our code in buffer
            -- only result is useful so it's ok
            path <- Path.parseAbsFile inPath
            let dir  = Path.toFilePath $ Path.parent path
                file = Path.toFilePath $ Path.filename path
            liftIO $ Temp.withTempFile dir (file <> ".tmp") $ \tmpFile handle -> do
                Text.hPutStr handle source
                let backupFile = Path.toFilePath path <> ".backup"
                Dir.renameFile (Path.toFilePath path) backupFile
                Dir.renameFile tmpFile (Path.toFilePath path)
                Dir.removeFile backupFile
            replyOk req ()

handleCloseFile :: Request CloseFile.Request -> StateT Env BusT ()
handleCloseFile (Request _ _ (CloseFile.Request path)) = do
    Env.empireEnv . Empire.activeFiles . at path .= Nothing

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
