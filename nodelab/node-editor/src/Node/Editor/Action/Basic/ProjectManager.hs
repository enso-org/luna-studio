module Node.Editor.Action.Basic.ProjectManager where

import           Empire.API.Data.GraphLocation         (GraphLocation)
import qualified JS.GraphLocation                      as JS
import           Node.Editor.Action.Basic.DestroyGraph (destroyGraph)
import qualified Node.Editor.Action.Batch              as Batch
import           Node.Editor.Action.Command            (Command)
import           Node.Editor.Batch.Workspace           (currentLocation, uiGraphLocation)
import           Luna.Prelude
import           Node.Editor.State.Global              (State, workspace)


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
