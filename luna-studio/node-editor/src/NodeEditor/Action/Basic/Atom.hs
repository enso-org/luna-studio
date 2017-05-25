module NodeEditor.Action.Basic.Atom where

import           Common.Prelude
import           LunaStudio.Data.GraphLocation          (filePath)
import           NodeEditor.Action.Basic.ProjectManager (loadGraph)
import           NodeEditor.Action.Batch                (searchNodes)
import           NodeEditor.Action.Camera.Persistence   (saveCamera)
import           NodeEditor.Action.Command              (Command)
import           NodeEditor.Action.State.NodeEditor     (resetGraph)
import           NodeEditor.Batch.Workspace             (currentLocation, nodeSearcherData)
import qualified NodeEditor.Batch.Workspace             as Workspace
import           NodeEditor.State.Global                (State, workspace)


setFile :: FilePath -> Command State ()
setFile path = do
    saveCamera
    mayWorkspace <- use workspace
    let mayCurrentLoc = view currentLocation <$> mayWorkspace
        newWorkspace  = Workspace.mk path
        newLocation   = newWorkspace ^. currentLocation
    when (mayCurrentLoc /= Just newLocation) $ do
        workspace ?= newWorkspace
        nsData <- if Just path == (view filePath <$> mayCurrentLoc)
            then return . fromMaybe def $ view nodeSearcherData <$> mayWorkspace
            else searchNodes >> return def
        workspace . _Just . nodeSearcherData .= nsData
        loadGraph newLocation

unsetFile :: Command State ()
unsetFile = do
    saveCamera
    workspace .= def
    resetGraph
