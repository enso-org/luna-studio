module Luna.Studio.Action.Basic.ProjectManager where

import           Empire.API.Data.Breadcrumb        (BreadcrumbItem, items)
import qualified Empire.API.Data.GraphLocation     (breadcrumb, items)
import           Empire.API.Data.Node              (Node, nodeId)
import           Luna.Studio.Action.Command        (Command)
import           Luna.Studio.Action.ProjectManager (navigateToGraph)
import           Luna.Studio.Batch.Workspace       (currentLocation)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global          (State, workspace)

loadProject :: ProjectId -> Command State ()
loadProject projId = navigateToGraph $ GraphLocation projId 0 (Breadcrumb [])

loadGraph :: GraphLocation -> Command State ()
loadGraph location = do
    destroyGraph
    workspace . currentLocation .= location
    saveCurrentLocation
    Batch.getProgram

navigateToGraph :: GraphLocation -> Command State ()
navigateToGraph location = do
    currentLoc <- use $ workspace . currentLocation
    when (currentLoc /= location) $ loadGraph location

saveCurrentLocation :: Command State ()
saveCurrentLocation = do
    workspace' <- use workspace
    liftIO $ JS.saveLocation $ workspace' ^. uiGraphLocation
