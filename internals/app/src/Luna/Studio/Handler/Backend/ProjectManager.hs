module Luna.Studio.Handler.Backend.ProjectManager
    ( handle
    ) where

import qualified Data.Map.Lazy                      as Map
import qualified Data.UUID.Types                    as UUID
import qualified Empire.API.Atom.OpenFile           as OpenFile
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
import qualified Luna.Studio.Action.Batch           as BatchCmd (importProject, closeFile, openFile, saveFile, setProject)
import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.Batch.Workspace        as Workspace
import qualified Luna.Studio.Event.Batch            as Batch
import qualified Luna.Studio.Event.CustomEvent      as CustomEvent
import qualified Luna.Studio.Event.Event                as Event
import           Luna.Studio.Event.Event            (Event (Batch, CustomEvent, Atom))
import           Luna.Studio.Event.Internal         (InternalEvent(..), ActionType(..))
import           Luna.Studio.Handler.Backend.Common (doNothing, handleResponse)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global


-- setProject :: ProjectId -> Project -> Command State ()
-- setProject projectId project = do
--     Global.workspace . Workspace.projects . at projectId ?= project
--     loadProject projectId

handle :: Event -> Maybe (Command State ())

handle (Event.Atom (InternalEvent SetProject path)) = Just $ BatchCmd.setProject path
handle (Event.Atom (InternalEvent CloseFile path))  = Just $ BatchCmd.closeFile path
handle (Event.Atom (InternalEvent OpenFile path))   = Just $ BatchCmd.openFile path
handle (Event.Atom (InternalEvent SaveFile path))   = Just $ BatchCmd.saveFile path

handle (Batch (Batch.ProjectSet response))    = Just $ handleResponse response doNothing doNothing

-- handle (Batch (Batch.FileOpened response)) = Just $ handleResponse response $ \(OpenFile.Request path) _ -> do --TODO odkomentowac jak już graph location sie zmieni jak trzeba
--     let location = GraphLocation.GraphLocation path (Breadcrumb [])
--     loadGraph location

handle (Batch (Batch.FileClosed response))    = Just $ handleResponse response doNothing doNothing

handle (Batch (Batch.FileSaved response))     = Just $ handleResponse response doNothing doNothing

handle _ = Nothing
