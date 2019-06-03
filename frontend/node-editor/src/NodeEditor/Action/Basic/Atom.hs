module NodeEditor.Action.Basic.Atom where

import Common.Prelude

import qualified NodeEditor.Batch.Workspace as Workspace

import Common.Action.Command                  (Command)
import LunaStudio.Data.GraphLocation          (filePath)
import NodeEditor.Action.Basic.ProjectManager (getSettings, loadGraph,
                                               saveSettings)
import NodeEditor.Action.State.App            (getWorkspace, modifyApp)
import NodeEditor.Action.State.NodeEditor     (resetApp)
import NodeEditor.Batch.Workspace             (currentLocation)
import NodeEditor.React.Model.App             (workspace)
import NodeEditor.State.Global                (State)
import Path                                   (Path, Rel, File)


setFile :: Path Rel File -> Command State ()
setFile path = do
    saveSettings
    mayWorkspace <- getWorkspace
    let mayCurrentLocation = mayWorkspace ^? _Just . currentLocation
        mayCurrentFilePath = view filePath <$> mayCurrentLocation
    when (Just path /= mayCurrentFilePath) $ do
        settings <- getSettings
        let newWorkspace = Workspace.mk path
        modifyApp $ workspace ?= newWorkspace
        loadGraph (newWorkspace ^. currentLocation) ((, settings) <$> mayCurrentLocation) True


updateFilePath :: Path Rel File -> Command State ()
updateFilePath path = do
    mayWorkspace <- getWorkspace
    let mayOldLocation = mayWorkspace ^? _Just . currentLocation
        mayOldFilePath = view filePath <$> mayOldLocation
    when (Just path /= mayOldFilePath) $ do
        let newWorkspace = Workspace.mk path
            newLocation = newWorkspace ^. currentLocation
        modifyApp $ workspace ?= newWorkspace
        saveSettings
        settings <- getSettings
        loadGraph newLocation (Just (newLocation, settings)) True


unsetFile :: Command State ()
unsetFile = do
    saveSettings
    modifyApp $ workspace .= def
    resetApp
