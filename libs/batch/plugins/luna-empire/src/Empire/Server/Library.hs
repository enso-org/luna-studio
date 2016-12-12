{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Library where

import           Prologue

import           Control.Monad.State              (StateT)
import qualified Empire.API.Library.CreateLibrary as CreateLibrary
import qualified Empire.API.Library.ListLibraries as ListLibraries
import           Empire.API.Request               (Request (..))
import qualified Empire.Commands.Library          as Library
import qualified Empire.Data.Library              as DataLibrary
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import           Empire.Server.Server             (errorMessage, replyFail, replyResult, sendToBus')
import qualified System.Log.MLogger               as Logger
import           ZMQ.Bus.Trans                    (BusT (..))

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)


handleCreateLibrary :: Request CreateLibrary.Request -> StateT Env BusT ()
handleCreateLibrary req@(Request _ request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.createLibrary
        (request ^. CreateLibrary.projectId)
        (request ^. CreateLibrary.libraryName)
        (fromString $ request ^. CreateLibrary.path)
    case result of
        Left err -> replyFail logger err req
        Right (libraryId, library) -> do
            Env.empireEnv .= newEmpireEnv
            replyResult req () $ CreateLibrary.Result libraryId $ DataLibrary.toAPI library
            sendToBus' $ CreateLibrary.Update libraryId $ DataLibrary.toAPI library

handleListLibraries :: Request ListLibraries.Request -> StateT Env BusT ()
handleListLibraries req@(Request _ request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.listLibraries
        (request ^. ListLibraries.projectId)
    case result of
        Left err -> replyFail logger err req
        Right librariesList -> do
            Env.empireEnv .= newEmpireEnv
            replyResult req () $ ListLibraries.Result $ (_2 %~ DataLibrary.toAPI) <$> librariesList
