module NodeEditor.Action.Basic.Atom where

import           Common.Prelude
import           Empire.API.Data.GraphLocation          (GraphLocation (GraphLocation))
import           NodeEditor.Action.Basic.ProjectManager (loadGraph)
import           NodeEditor.Action.Command              (Command)
import           NodeEditor.Action.State.NodeEditor     (resetGraph)
import           NodeEditor.Batch.Workspace             (currentLocation)
import qualified NodeEditor.Batch.Workspace             as Workspace
import           NodeEditor.State.Global                (State, workspace)


openFile :: FilePath -> Command State ()
openFile path = do
    mayCurrentLoc <- preuse $ workspace . traverse . currentLocation
    let newWorkspace = Workspace.mk path
        newLocation = newWorkspace ^. currentLocation
    when (mayCurrentLoc /= Just newLocation) $ do
        workspace ?= newWorkspace
        loadGraph newLocation

closeFile :: Command State ()
closeFile = do
    workspace .= def
    resetGraph
