{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Server.Library where

import           Prologue

import           Control.Lens                         ((.=), (^..), to, use)
import           Control.Monad.Catch                  (try)
import           Control.Monad.State                  (StateT)
import           Empire.Commands.Graph                (prepareLunaError)
import qualified Empire.Commands.Library              as Library
import           Empire.Data.AST                      (SomeASTException)
import qualified Empire.Data.Library                  as DataLibrary
import qualified Empire.Empire                        as Empire
import           Empire.Env                           (Env)
import qualified Empire.Env                           as Env
import           Empire.Server.Server                 (replyFail, replyResult, sendToBus')
import qualified LunaStudio.API.Library.CreateLibrary as CreateLibrary
import qualified LunaStudio.API.Library.ListLibraries as ListLibraries
import           LunaStudio.API.Request               (Request (..))
import qualified LunaStudio.API.Response              as Response
import qualified System.Log.MLogger                   as Logger
import           ZMQ.Bus.Trans                        (BusT (..))


logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)


handleCreateLibrary :: Request CreateLibrary.Request -> StateT Env BusT ()
handleCreateLibrary req@(Request _ _ request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result           <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.createLibrary
        (request ^. CreateLibrary.libraryName)
        (fromString $ request ^. CreateLibrary.path)
    case result of
        Left (exc :: SomeASTException) -> do
            err <- liftIO $ prepareLunaError $ toException exc
            replyFail logger err req (Response.Error err)
        Right (library, newEmpireEnv) -> do
            Env.empireEnv .= newEmpireEnv
            replyResult req () $ CreateLibrary.Result $_NOT_IMPLEMENTED $ DataLibrary.toAPI library
            sendToBus' $ CreateLibrary.Update $_NOT_IMPLEMENTED $ DataLibrary.toAPI library

handleListLibraries :: Request ListLibraries.Request -> StateT Env BusT ()
handleListLibraries req@(Request _ _ request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    result           <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.listLibraries
    case result of
        Left (exc :: SomeASTException) -> do
            err <- liftIO $ prepareLunaError $ toException exc
            replyFail logger err req (Response.Error err)
        Right (librariesList, newEmpireEnv) -> do
            Env.empireEnv .= newEmpireEnv
            let libraries = zip [0..] (map DataLibrary.toAPI librariesList)
            replyResult req () $ ListLibraries.Result $ libraries
