{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Server.Library where

import Prologue

import qualified Bus.Framework.App                    as Bus
import qualified Empire.Commands.Library              as Library
import qualified Empire.Data.Library                  as DataLibrary
import qualified Empire.Empire                        as Empire
import qualified Empire.Env                           as Env
import qualified LunaStudio.API.Library.CreateLibrary as CreateLibrary
import qualified LunaStudio.API.Library.ListLibraries as ListLibraries
import qualified LunaStudio.API.Response              as Response
import qualified System.Log.MLogger                   as Logger

import Control.Lens           (to, use, (.=), (^..))
import Control.Monad.Catch    (try)
import Control.Monad.State    (StateT)
import Empire.Commands.Graph  (prepareLunaError)
import Empire.Data.AST        (SomeASTException)
import Empire.Env             (Env)
import Empire.Server.Server   (replyFail, replyResult, sendToBus')
import LunaStudio.API.Request (Request (..))


logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)


handleCreateLibrary :: Request CreateLibrary.Request -> StateT Env Bus.App ()
handleCreateLibrary req@(Request _ _ request) = $_NOT_IMPLEMENTED -- do
    {-currentEmpireEnv <- use Env.empireEnv-}
    {-empireNotifEnv   <- use Env.empireNotif-}
    {-result           <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.createLibrary-}
        {-(request ^. CreateLibrary.libraryName)-}
        {-(fromString $ request ^. CreateLibrary.path)-}
    {-case result of-}
        {-Left (exc :: SomeASTException) -> do-}
            {-err <- liftIO $ prepareLunaError $ toException exc-}
            {-replyFail logger err req (Response.Error err)-}
        {-Right (library, newEmpireEnv) -> do-}
    {-replyResult req () $ CreateLibrary.Result $_NOT_IMPLEMENTED $_NOT_IMPLEMENTED-}
    {-sendToBus' $ CreateLibrary.Update $_NOT_IMPLEMENTED $_NOT_IMPLEMENTED-}

handleListLibraries :: Request ListLibraries.Request -> StateT Env Bus.App ()
handleListLibraries req@(Request _ _ request) = $_NOT_IMPLEMENTED -- do
    {-currentEmpireEnv <- use Env.empireEnv-}
    {-empireNotifEnv   <- use Env.empireNotif-}
    {-result           <- liftIO $ try $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.listLibraries-}
    {-case result of-}
        {-Left (exc :: SomeASTException) -> do-}
            {-err <- liftIO $ prepareLunaError $ toException exc-}
            {-replyFail logger err req (Response.Error err)-}
        {-Right (librariesList, newEmpireEnv) -> do-}
            {-Env.empireEnv .= newEmpireEnv-}
            {-replyResult req () $ ListLibraries.Result $_NOT_IMPLEMENTED-}
