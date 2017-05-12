{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Server.Library where

import           Prologue

import           Control.Monad.Catch              (try)
import           Control.Monad.State              (StateT)
import qualified Empire.API.Library.CreateLibrary as CreateLibrary
import qualified Empire.API.Library.ListLibraries as ListLibraries
import           Empire.API.Request               (Request (..))
import qualified Empire.API.Response              as Response
import qualified Empire.Commands.Library          as Library
import           Empire.Data.AST                  (SomeASTException)
import qualified Empire.Data.Library              as DataLibrary
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import           Empire.Server.Server             (replyFail, replyResult, sendToBus')
import qualified System.Log.MLogger               as Logger
import           ZMQ.Bus.Trans                    (BusT (..))

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)


handleCreateLibrary :: Request CreateLibrary.Request -> StateT Env BusT ()
handleCreateLibrary req@(Request _ _ request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result           <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.createLibrary
        (request ^. CreateLibrary.libraryName)
        (fromString $ request ^. CreateLibrary.path)
        ""
    case result of
        Left (exc :: SomeASTException) ->
            let err = displayException exc in replyFail logger err req (Response.Error err)
        Right (library, newEmpireEnv) -> do
            Env.empireEnv .= newEmpireEnv
            replyResult req () $ CreateLibrary.Result $notImplemented $ DataLibrary.toAPI library
            sendToBus' $ CreateLibrary.Update $notImplemented $ DataLibrary.toAPI library

handleListLibraries :: Request ListLibraries.Request -> StateT Env BusT ()
handleListLibraries req@(Request _ _ request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result           <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.listLibraries
    case result of
        Left (exc :: SomeASTException) ->
            let err = displayException exc in replyFail logger err req (Response.Error err)
        Right (librariesList, newEmpireEnv) -> do
            Env.empireEnv .= newEmpireEnv
            let libraries = zip [0..] (map DataLibrary.toAPI librariesList)
            replyResult req () $ ListLibraries.Result $ libraries
