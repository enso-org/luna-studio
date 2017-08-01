module NodeEditor.Action.Basic.Atom where

import           Common.Action.Command                  (Command)
import           Common.Prelude
import           LunaStudio.Data.GraphLocation          (filePath)
import           NodeEditor.Action.Basic.ProjectManager (getSettings, loadGraph, saveSettings)
import           NodeEditor.Action.State.NodeEditor     (resetApp)
import           NodeEditor.Batch.Workspace             (currentLocation)
import qualified NodeEditor.Batch.Workspace             as Workspace
import           NodeEditor.State.Global                (State, workspace)


setFile :: FilePath -> Command State ()
setFile path = do
    saveSettings
    mayCurrentLocation <- preuse $ workspace . traverse . currentLocation
    let mayCurrentFilePath = view filePath <$> mayCurrentLocation
    when (Just path /= mayCurrentFilePath) $ do
        settings <- getSettings
        let newWorkspace = Workspace.mk path
        workspace ?= newWorkspace
        loadGraph (newWorkspace ^. currentLocation) $ (, settings) <$> mayCurrentLocation

unsetFile :: Command State ()
unsetFile = do
    saveSettings
    workspace .= def
    resetApp
