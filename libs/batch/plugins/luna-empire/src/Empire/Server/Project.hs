{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Project where

import           Prologue

import           Control.Monad.State              (StateT)
import qualified Empire.API.Project.CreateProject as CreateProject
import qualified Empire.API.Project.ExportProject as ExportProject
import qualified Empire.API.Project.ImportProject as ImportProject
import qualified Empire.API.Project.ListProjects  as ListProjects
import           Empire.API.Request               (Request (..))
import qualified Empire.Commands.Library          as Library
import qualified Empire.Commands.Persistence      as Persistence
import qualified Empire.Commands.Project          as Project
import qualified Empire.Data.Project              as DataProject
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import           Empire.Server.Server             (replyFail, replyResult, sendToBus')
import qualified System.Log.MLogger               as Logger
import           ZMQ.Bus.Trans                    (BusT (..))

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)


handleCreateProject :: Request CreateProject.Request -> StateT Env BusT ()
handleCreateProject req@(Request _ request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ do
      (projectId, project) <- Project.createProject Nothing (request ^. CreateProject.name)
      (libraryId, library) <- Library.createLibrary projectId (Just "Main") "Main.luna"

      let project' = project & DataProject.libs . at libraryId ?~ library
      return (projectId, project')
    case result of
        Left err -> replyFail logger err req
        Right (projectId, project) -> do
            Env.empireEnv .= newEmpireEnv
            replyResult req () $ CreateProject.Result projectId $ DataProject.toAPI project
            sendToBus' $ CreateProject.Update projectId $ DataProject.toAPI project

handleListProjects :: Request ListProjects.Request -> StateT Env BusT ()
handleListProjects req = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Project.listProjects
    case result of
        Left err -> replyFail logger err req
        Right projectList -> do
            Env.empireEnv .= newEmpireEnv
            replyResult req () $ ListProjects.Result $ (_2 %~ DataProject.toAPI) <$> projectList

sendListProjectsUpdate :: StateT Env BusT ()
sendListProjectsUpdate = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Project.listProjects
    case result of
        Left err -> logger Logger.error err
        Right projectList -> do
            Env.empireEnv .= newEmpireEnv
            sendToBus' $ ListProjects.Update $ (_2 %~ DataProject.toAPI) <$> projectList

handleExportProject :: Request ExportProject.Request -> StateT Env BusT ()
handleExportProject req@(Request _ (ExportProject.Request projectId)) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Persistence.exportProject projectId
    case result of
        Left err -> replyFail logger err req
        Right projectData -> do
            replyResult req () $ ExportProject.Result projectData

handleImportProject :: Request ImportProject.Request -> StateT Env BusT ()
handleImportProject req@(Request _ (ImportProject.Request projectData)) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Persistence.importProject projectData
    case result of
        Left err -> replyFail logger err req
        Right (projectId, project) -> do
            Env.empireEnv .= newEmpireEnv
            replyResult req () $ ImportProject.Result projectId $ DataProject.toAPI project
            sendToBus' $ CreateProject.Update projectId $ DataProject.toAPI project
            sendListProjectsUpdate
