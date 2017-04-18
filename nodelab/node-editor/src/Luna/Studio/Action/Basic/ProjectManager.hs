module Luna.Studio.Action.Basic.ProjectManager where

import           Empire.API.Data.GraphLocation         (GraphLocation)
import qualified JS.GraphLocation                      as JS
import           Luna.Studio.Action.Basic.DestroyGraph (destroyGraph)
import qualified Luna.Studio.Action.Batch              as Batch
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Batch.Workspace           (currentLocation, uiGraphLocation)
import           Luna.Prelude
import           Luna.Studio.State.Global              (State, workspace)


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
