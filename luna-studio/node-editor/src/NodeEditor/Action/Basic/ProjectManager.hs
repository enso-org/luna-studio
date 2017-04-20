module NodeEditor.Action.Basic.ProjectManager where

import           Empire.API.Data.GraphLocation         (GraphLocation)
import qualified JS.GraphLocation                      as JS
import           NodeEditor.Action.Basic.DestroyGraph (destroyGraph)
import qualified NodeEditor.Action.Batch              as Batch
import           NodeEditor.Action.Command            (Command)
import           NodeEditor.Batch.Workspace           (currentLocation, uiGraphLocation)
import           Common.Prelude
import           NodeEditor.State.Global              (State, workspace)


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
