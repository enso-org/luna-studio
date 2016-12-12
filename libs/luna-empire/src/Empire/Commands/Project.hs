module Empire.Commands.Project
    ( withProject
    , listProjects
    , createProject
    ) where

import           Control.Monad.Except    (throwError)
import           Control.Monad.Reader
import           Control.Monad.State
import           Prologue

import           Empire.API.Data.Project (ProjectId)
import           Empire.Data.Project     (Project)
import qualified Empire.Data.Project     as Project
import           Empire.Empire           (Command, Empire, ProjectManager)
import qualified Empire.Empire           as Empire

import qualified Data.Map                as Map
import qualified Data.UUID.V4            as UUID

createProject :: Maybe ProjectId -> String -> Empire (ProjectId, Project)
createProject maybePid name = do
    let project = Project.make name
    pid <- case maybePid of
      Just pid -> do
        Empire.projectManager . at pid ?= project
        return pid
      Nothing -> zoom Empire.projectManager $ insertAtNewId project
    return (pid, project)

listProjects :: Empire [(ProjectId, Project)]
listProjects = uses Empire.projectManager Map.toList

withProject :: ProjectId -> Command Project a -> Empire a
withProject pid cmd = zoom (Empire.projectManager . at pid) $ do
    projectMay <- get
    notifEnv   <- ask
    case projectMay of
        Nothing      -> throwError $ "Project " ++ (show pid) ++ " does not exist."
        Just project -> do
            let result = (_2 %~ Just) <$> Empire.runEmpire notifEnv project cmd
            Empire.empire $ const $ const result

-- internal

insertAtNewId :: Project -> Command ProjectManager ProjectId
insertAtNewId project = do
    key <- liftIO $ UUID.nextRandom
    at key ?= project
    return key
