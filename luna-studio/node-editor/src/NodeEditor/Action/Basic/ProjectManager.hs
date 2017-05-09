module NodeEditor.Action.Basic.ProjectManager where

import           Common.Prelude
import           Empire.API.Data.GraphLocation      (GraphLocation)
import qualified JS.GraphLocation                   as JS
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.NodeEditor (resetGraph)
import           NodeEditor.Batch.Workspace         (currentLocation, uiGraphLocation)
import           NodeEditor.State.Global            (State, workspace)


loadGraph :: GraphLocation -> Command State ()
loadGraph location = do
    resetGraph
    workspace . _Just . currentLocation .= location
    saveCurrentLocation
    Batch.getProgram

navigateToGraph :: GraphLocation -> Command State ()
navigateToGraph location =
    withJustM (preuse $ workspace . traverse . currentLocation) $ \currentLoc ->
        when (currentLoc /= location) $ loadGraph location

saveCurrentLocation :: Command State ()
saveCurrentLocation =
    withJustM (preuse $ workspace . traverse . uiGraphLocation) $
        liftIO . JS.saveLocation
