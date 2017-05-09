module Empire.Server.Atom where

import           Control.Monad.State                   (StateT)
import qualified Data.Text.IO                          as Text
import           Prologue                              hiding (Item)
import qualified Path
import qualified System.Directory                      as Dir
import qualified System.IO.Temp                        as Temp

import           Empire.Env                            (Env)
import qualified Empire.Env                            as Env

import           Empire.API.Request                    (Request (..))
import qualified Empire.API.Atom.SetProject            as SetProject
import qualified Empire.API.Atom.OpenFile              as OpenFile
import qualified Empire.API.Atom.SaveFile              as SaveFile
import qualified Empire.API.Atom.CloseFile             as CloseFile
import qualified Empire.API.Atom.GetBuffer             as GetBuffer
import qualified Empire.API.Atom.IsSaved               as IsSaved
import qualified Empire.API.Atom.Substitute            as Substitute
import qualified Empire.API.Response                   as Response

import qualified Empire.Commands.Graph                 as Graph
import qualified Empire.Commands.Library               as Library
import qualified Empire.Data.Library                   as Library
import qualified Empire.Empire                         as Empire
import           Empire.Server.Server                  (errorMessage, replyFail, replyOk)
import qualified System.Log.MLogger                    as Logger
import           ZMQ.Bus.Trans                         (BusT (..))

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

handleSetProject :: Request SetProject.Request -> StateT Env BusT ()
handleSetProject a = return ()

handleOpenFile :: Request OpenFile.Request -> StateT Env BusT ()
handleOpenFile req@(Request _ _ (OpenFile.Request path)) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Graph.openFile path
    case result of
        Left err -> replyFail logger err req (Response.Error err)
        Right _  -> do
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
