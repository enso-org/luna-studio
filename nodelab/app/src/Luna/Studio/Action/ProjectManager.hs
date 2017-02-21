module Luna.Studio.Action.ProjectManager
    ( loadProject
    , setCurrentBreadcrumb
    , navigateToGraph
    , enterBreadcrumbs
    , loadGraph
    ) where

import           Luna.Studio.Prelude

import           Empire.API.Data.Breadcrumb       (Breadcrumb (..), BreadcrumbItem, Named)
import           Empire.API.Data.GraphLocation    (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation    as GraphLocation
import           Empire.API.Data.Project          (ProjectId)
import qualified JS.GraphLocation                 as JS
import qualified Luna.Studio.Action.Batch         as BatchCmd
import qualified Luna.Studio.Action.Breadcrumbs   as Breadcrumbs
import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Action.Graph.Destroy (destroyGraph)
import qualified Luna.Studio.Batch.Workspace      as Workspace
import           Luna.Studio.State.Global         (State)
import qualified Luna.Studio.State.Global         as Global


loadProject :: ProjectId -> Command State ()
loadProject projId = do
    let newLocation = GraphLocation projId 0 (Breadcrumb [])
    navigateToGraph newLocation

loadGraph :: GraphLocation -> Command State ()
loadGraph location = do
    destroyGraph
    Global.workspace . Workspace.currentLocation .= location
    saveCurrentLocation
    BatchCmd.getProgram

navigateToGraph :: GraphLocation -> Command State ()
navigateToGraph location = do
    currentLocation <- use $ Global.workspace . Workspace.currentLocation
    when (currentLocation /= location) $ loadGraph location

setCurrentBreadcrumb :: Breadcrumb (Named BreadcrumbItem) -> Command State ()
setCurrentBreadcrumb = Breadcrumbs.set

enterBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command State ()
enterBreadcrumbs newBc = do
    location <- use $ Global.workspace . Workspace.currentLocation
    let newLocation = location & GraphLocation.breadcrumb .~ newBc
    navigateToGraph newLocation

saveCurrentLocation :: Command State ()
saveCurrentLocation = do
    workspace <- use Global.workspace
    liftIO $ JS.saveLocation $ workspace ^. Workspace.uiGraphLocation
