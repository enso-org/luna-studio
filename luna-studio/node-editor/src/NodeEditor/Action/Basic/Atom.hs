module NodeEditor.Action.Basic.Atom where

import           Common.Prelude
import           NodeEditor.Action.Basic.ProjectManager (loadGraph)
import           NodeEditor.Action.Camera.Persistence   (saveCamera)
import           NodeEditor.Action.Command              (Command)
import           NodeEditor.Action.State.NodeEditor     (resetGraph)
import           NodeEditor.Batch.Workspace             (currentLocation)
import qualified NodeEditor.Batch.Workspace             as Workspace
import           NodeEditor.State.Global                (State, workspace)


setFile :: FilePath -> Command State ()
setFile path = do
    saveCamera
    mayCurrentLoc <- preuse $ workspace . traverse . currentLocation
    let newWorkspace = Workspace.mk path
        newLocation = newWorkspace ^. currentLocation
    when (mayCurrentLoc /= Just newLocation) $ do
        workspace ?= newWorkspace
        loadGraph newLocation

unsetFile :: Command State ()
unsetFile = do
    saveCamera
    workspace .= def
    resetGraph
