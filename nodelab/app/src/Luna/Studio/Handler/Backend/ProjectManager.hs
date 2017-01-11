module Luna.Studio.Handler.Backend.ProjectManager
    ( toAction
    ) where

import qualified Data.Map.Lazy                      as Map
import qualified Data.Text                          as Text
import qualified Data.UUID.Types                    as UUID
import           GHCJS.Marshal.Pure                 (pFromJSVal)
import           Luna.Studio.Prelude

import           Empire.API.Data.Breadcrumb         (Breadcrumb (..))
import qualified Empire.API.Data.GraphLocation      as GraphLocation
import qualified Empire.API.Project.CreateProject   as CreateProject
import qualified Empire.API.Project.ExportProject   as ExportProject
import qualified Empire.API.Project.ImportProject   as ImportProject
import qualified Empire.API.Project.ListProjects    as ListProjects

import qualified Event.Batch                        as Batch
import qualified Event.CustomEvent                  as CustomEvent
import           Event.Event                        (Event (Batch, CustomEvent))
import           JS.DownloadFile                    (downloadFile)
import qualified Luna.Studio.Action.Batch           as BatchCmd (importProject)
import           Luna.Studio.Action.Command         (Command, performIO)
import           Luna.Studio.Action.ProjectManager  (loadGraph, loadProject)
import qualified Luna.Studio.Batch.Workspace        as Workspace
import           Luna.Studio.Handler.Backend.Common (handleResponse)
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global

toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.ProjectList response)) = Just $ handleResponse response $ \_ (ListProjects.Result projects) -> do
    let projectsMap = Map.fromList projects
    Global.workspace . Workspace.projects .= projectsMap

    lastLocation <- use $ Global.workspace . Workspace.lastUILocation
    let fallbackLocation  = GraphLocation.GraphLocation (fromMaybe (error "No projects found") $ projects ^? ix 0 . _1) 0 (Breadcrumb [])
        lastGraphLocation = lastLocation >>= (Workspace.fromUIGraphLocation projectsMap)
    Global.workspace . Workspace.currentLocation .= fromMaybe fallbackLocation lastGraphLocation
    loc <- use $ Global.workspace . Workspace.currentLocation
    loadGraph loc

toAction (Batch (Batch.ProjectCreated response)) = Just $ handleResponse response $ \_ (CreateProject.Result projectId project) -> do
    Global.workspace . Workspace.projects . at projectId ?= project
    loadProject projectId

toAction (Batch (Batch.ProjectCreatedUpdate (CreateProject.Update projectId project))) = Just $ Global.workspace . Workspace.projects . at projectId ?= project

toAction (Batch (Batch.ProjectExported response)) = Just $ do
    handleResponse response $ \(ExportProject.Request uuid) (ExportProject.Result projectData) -> performIO $ downloadFile (Text.pack $ UUID.toString uuid <> ".lproj") projectData

toAction (Batch (Batch.ProjectImported response)) = Just $ do
    handleResponse response $ \_ (ImportProject.Result projectId project) -> do
        Global.workspace . Workspace.projects . at projectId ?= project
        loadProject projectId

toAction (CustomEvent (CustomEvent.RawEvent "file.import" jsVal)) = Just $ do
    let projectData = pFromJSVal jsVal :: String
    BatchCmd.importProject $ Text.pack projectData
toAction _ = Nothing
