module NodeEditor.Action.Basic.Atom where

import           Common.Prelude
import           LunaStudio.Data.GraphLocation          (filePath)
import           NodeEditor.Action.Basic.ProjectManager (getSettings, loadGraph, saveSettings)
import           NodeEditor.Action.Batch                (searchNodes)
import           NodeEditor.Action.Command              (Command)
import           NodeEditor.Action.State.NodeEditor     (resetApp)
import           NodeEditor.Batch.Workspace             (currentLocation, nodeSearcherData)
import qualified NodeEditor.Batch.Workspace             as Workspace
import           NodeEditor.State.Global                (State, workspace)


setFile :: FilePath -> Command State ()
setFile path = do
    saveSettings
    mayCurrentLocation <- preuse $ workspace . traverse . currentLocation
    let mayCurrentFilePath = view filePath <$> mayCurrentLocation
    mayNsData <- preuse $ workspace . traverse . nodeSearcherData
    when (Just path /= mayCurrentFilePath) $ do
        settings <- getSettings
        let newWorkspace = Workspace.mk path
        workspace ?= newWorkspace
        maybe searchNodes (workspace . _Just . nodeSearcherData .=) mayNsData
        loadGraph (newWorkspace ^. currentLocation) $ (, settings) <$> mayCurrentLocation

unsetFile :: Command State ()
unsetFile = do
    saveSettings
    workspace .= def
    resetApp
