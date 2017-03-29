module Luna.Studio.Handler.Backend.ProjectManager
    ( handle
    ) where

import qualified Data.Map.Lazy                      as Map
import qualified Data.UUID.Types                    as UUID
import           Empire.API.Data.Breadcrumb         (Breadcrumb (..))
import qualified Empire.API.Data.GraphLocation      as GraphLocation
import           Empire.API.Data.Project            (Project, ProjectId)
import qualified Empire.API.Project.CreateProject   as CreateProject
import qualified Empire.API.Project.ExportProject   as ExportProject
import qualified Empire.API.Project.ImportProject   as ImportProject
import qualified Empire.API.Project.ListProjects    as ListProjects
import qualified Empire.API.Project.OpenProject     as OpenProject
import qualified Empire.API.Response                as Response
import           GHCJS.Marshal.Pure                 (pFromJSVal)
import           JS.DownloadFile                    (downloadFile)
-- import           Luna.Studio.Action.Basic           (loadGraph, loadProject)
-- import qualified Luna.Studio.Action.Batch           as BatchCmd (importProject)
import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.Batch.Workspace        as Workspace
import qualified Luna.Studio.Event.Batch            as Batch
import qualified Luna.Studio.Event.CustomEvent      as CustomEvent
import           Luna.Studio.Event.Event            (Event (Batch, CustomEvent))
import           Luna.Studio.Handler.Backend.Common (doNothing, handleResponse)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global


-- setProject :: ProjectId -> Project -> Command State ()
-- setProject projectId project = do
--     Global.workspace . Workspace.projects . at projectId ?= project
--     loadProject projectId

handle :: Event -> Maybe (Command State ())
-- handle (Batch (Batch.ProjectList response)) = Just $ handleResponse response success doNothing where
--     success result = do
--         let projects    = result ^. ListProjects.projects
--             projectsMap = Map.fromList projects
--         Global.workspace . Workspace.projects .= projectsMap
--
--         lastLocation <- use $ Global.workspace . Workspace.lastUILocation
--         let fallbackLocation  = GraphLocation.GraphLocation (fromMaybe (error "No projects found") $ projects ^? ix 0 . _1) 0 (Breadcrumb [])
--             lastGraphLocation = lastLocation >>= Workspace.fromUIGraphLocation projectsMap
--         Global.workspace . Workspace.currentLocation .= fromMaybe fallbackLocation lastGraphLocation
--         loc <- use $ Global.workspace . Workspace.currentLocation
--         loadGraph loc
--
-- handle (Batch (Batch.ProjectOpened response)) = Just $ handleResponse response success doNothing where
--     success result = do
--         let projectId = result ^. OpenProject.projectId
--             project   = result ^. OpenProject.project
--         setProject projectId project
--
-- handle (Batch (Batch.ProjectOpenedUpdate update)) = Just $ Global.workspace . Workspace.projects . at projectId ?= project where
--     projectId = update ^. OpenProject.projectId'
--     project   = update ^. OpenProject.project'
--
-- handle (Batch (Batch.ProjectCreated response)) = Just $ handleResponse response success doNothing where
--     success result = do
--         let projectId = result ^. CreateProject.projectId
--             project   = result ^. CreateProject.project
--         setProject projectId project
--
-- handle (Batch (Batch.ProjectCreatedUpdate update)) = Just $ Global.workspace . Workspace.projects . at projectId ?= project where
--     projectId = update ^. CreateProject.projectId'
--     project   = update ^. CreateProject.project'
--
-- handle (Batch (Batch.ProjectExported response)) = Just $ handleResponse response success doNothing where
--     projectId = response ^. Response.request . ExportProject.projectId
--     success result = do
--         let projectData = result ^. ExportProject.projectData
--         liftIO $ downloadFile (convert $ UUID.toString projectId <> ".lproj") projectData
--
-- handle (Batch (Batch.ProjectImported response)) = Just $ handleResponse response success doNothing where
--     success result = do
--         let projectId = result ^. ImportProject.projectId
--             project   = result ^. ImportProject.project
--         setProject projectId project

-- handle (CustomEvent (CustomEvent.RawEvent "file.import" jsVal)) = Just $ BatchCmd.importProject $ convert projectData where
--     projectData = pFromJSVal jsVal :: String

handle _ = Nothing
