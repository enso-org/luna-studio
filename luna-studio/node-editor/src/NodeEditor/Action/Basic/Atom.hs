module NodeEditor.Action.Basic.Atom where

import           Common.Prelude
import           NodeEditor.Action.Basic.ProjectManager (loadGraph)
import           NodeEditor.Action.Camera.Persistence   (saveCamera)
import           NodeEditor.Action.Command              (Command)
import           NodeEditor.Action.State.NodeEditor     (resetGraph)
import           NodeEditor.Batch.Workspace             (currentLocation)
import qualified NodeEditor.Batch.Workspace             as Workspace
import           NodeEditor.State.Global                (State, workspace)


openFile :: FilePath -> Command State ()
openFile path = do
    saveCamera
    mayCurrentLoc <- preuse $ workspace . traverse . currentLocation
    let newWorkspace = Workspace.mk path
        newLocation = newWorkspace ^. currentLocation
    when (mayCurrentLoc /= Just newLocation) $ do
        workspace ?= newWorkspace
        loadGraph newLocation

closeFile :: Command State ()
closeFile = do
    saveCamera
    workspace .= def
    resetGraph
