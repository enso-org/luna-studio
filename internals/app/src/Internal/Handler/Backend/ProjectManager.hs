module Internal.Handler.Backend.ProjectManager
    ( handle
    ) where

import qualified Data.Map.Lazy                      as Map
import qualified Data.UUID.Types                    as UUID
import qualified Empire.API.Atom.OpenFile           as OpenFile
import           Empire.API.Data.Breadcrumb         (Breadcrumb (..))
import qualified Empire.API.Data.GraphLocation      as GraphLocation
import           Empire.API.Data.Project            (Project, ProjectId)
-- import qualified Empire.API.Project.CreateProject   as CreateProject
-- import qualified Empire.API.Project.ExportProject   as ExportProject
-- import qualified Empire.API.Project.ImportProject   as ImportProject
-- import qualified Empire.API.Project.ListProjects    as ListProjects
-- import qualified Empire.API.Project.OpenProject     as OpenProject
import qualified Empire.API.Response                as Response
import           GHCJS.Marshal.Pure                 (pFromJSVal)
import           JS.DownloadFile                    (downloadFile)
-- import           Internal.Action.Basic           (loadGraph, loadProject)
import qualified Internal.Action.Batch           as BatchCmd (closeFile, isSaved, openFile, saveFile, setProject)
import           Internal.Action.Command         (Command)
import qualified Internal.Batch.Workspace        as Workspace
import qualified Internal.Event.Batch            as Batch
import qualified Internal.Event.CustomEvent      as CustomEvent
import qualified Internal.Event.Event                as Event
import           Internal.Event.Event            (Event (Batch, CustomEvent, Atom))
import           Internal.Event.Internal         (InternalEvent(..), ActionType(..))
import           Internal.Handler.Backend.Common (doNothing, handleResponse)
import           Internal.Prelude
import           Internal.State.Global           (State)
import qualified Internal.State.Global           as Global


-- setProject :: ProjectId -> Project -> Command State ()
-- setProject projectId project = do
--     Global.workspace . Workspace.projects . at projectId ?= project
--     loadProject projectId

handle :: Event -> Maybe (Command State ())

handle (Event.Atom (InternalEvent SetProject path)) = Just $ BatchCmd.setProject path
handle (Event.Atom (InternalEvent CloseFile path))  = Just $ BatchCmd.closeFile path
handle (Event.Atom (InternalEvent OpenFile path))   = Just $ BatchCmd.openFile path
handle (Event.Atom (InternalEvent SaveFile path))   = Just $ BatchCmd.saveFile path
handle (Event.Atom (InternalEvent IsSaved path))    = Just $ BatchCmd.isSaved path

handle (Batch (Batch.ProjectSet response))    = Just $ handleResponse response doNothing doNothing

-- handle (Batch (Batch.FileOpened response)) = Just $ handleResponse response $ \(OpenFile.Request path) _ -> do --TODO odkomentowac jak już graph location sie zmieni jak trzeba
--     let location = GraphLocation.GraphLocation path (Breadcrumb [])
--     loadGraph location

handle (Batch (Batch.FileClosed response))    = Just $ handleResponse response doNothing doNothing

handle (Batch (Batch.FileSaved response))     = Just $ handleResponse response doNothing doNothing

handle _ = Nothing
