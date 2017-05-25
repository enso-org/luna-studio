{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Server.Atom where

import           Control.Monad.Catch            (try)
import           Control.Monad.State            (StateT)
import qualified Data.Text.IO                   as Text
import qualified Path
import           Prologue                       hiding (Item)
import qualified System.Directory               as Dir
import qualified System.IO.Temp                 as Temp

import           Empire.Env                     (Env)
import qualified Empire.Env                     as Env

import qualified LunaStudio.API.Atom.CloseFile  as CloseFile
import qualified LunaStudio.API.Atom.IsSaved    as IsSaved
import qualified LunaStudio.API.Atom.OpenFile   as OpenFile
import qualified LunaStudio.API.Atom.SaveFile   as SaveFile
import qualified LunaStudio.API.Atom.SetProject as SetProject
import qualified LunaStudio.API.Atom.Substitute as Substitute
import           LunaStudio.API.Request         (Request (..))
import qualified LunaStudio.API.Response        as Response

import qualified Empire.Commands.Graph          as Graph
import qualified Empire.Commands.Library        as Library
import           Empire.Data.AST                (SomeASTException)
import qualified Empire.Data.Library            as Library
import qualified Empire.Empire                  as Empire
import           Empire.Server.Server           (errorMessage, replyFail, replyOk)
import qualified System.Log.MLogger             as Logger
import           ZMQ.Bus.Trans                  (BusT (..))

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

handleSetProject :: Request SetProject.Request -> StateT Env BusT ()
handleSetProject a = return ()

handleOpenFile :: Request OpenFile.Request -> StateT Env BusT ()
handleOpenFile req@(Request _ _ (OpenFile.Request path)) = do
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
    source <- preuse (Env.empireEnv . Empire.activeFiles . at inPath . traverse . Library.code)
    case source of
        Nothing     -> logger Logger.error $ errorMessage <> inPath <> " is not open"
        Just source -> do
            path <- Path.parseAbsFile inPath
            let dir  = Path.toFilePath $ Path.parent path
                file = Path.toFilePath $ Path.filename path
            liftIO $ Temp.withTempFile dir (file ++ ".tmp") $ \tmpFile handle -> do
                Text.hPutStr handle source
                Dir.renameFile (Path.toFilePath path) (Path.toFilePath path ++ ".backup")
                Dir.renameFile tmpFile (Path.toFilePath path)
            replyOk req ()

handleCloseFile :: Request CloseFile.Request -> StateT Env BusT ()
handleCloseFile (Request _ _ (CloseFile.Request path)) = do
    Env.empireEnv . Empire.activeFiles . at path .= Nothing

handleIsSaved :: Request IsSaved.Request -> StateT Env BusT ()
handleIsSaved (Request _ _ _) = $notImplemented
