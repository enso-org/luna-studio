module NodeEditor.Action.Basic.ProjectManager where

import Common.Prelude

import qualified JS.Atom                 as Atom
import qualified LunaStudio.Data.Project as Project
import qualified NodeEditor.Action.Batch as Batch
import qualified NodeEditor.State.Global as Global

import Common.Action.Command              (Command)
import LunaStudio.Data.GraphLocation      (GraphLocation, filePath)
import LunaStudio.Data.Project            (LocationSettings (LocationSettings))
import NodeEditor.Action.State.App        (getWorkspace, modifyApp)
import NodeEditor.Action.State.NodeEditor (getNodeEditor, getScreenTransform, modifyNodeEditor, resetGraph, setGraphStatus)
import NodeEditor.Batch.Workspace         (currentLocation)
import NodeEditor.React.Model.App         (workspace)
import NodeEditor.React.Model.NodeEditor  (GraphStatus (GraphLoading), visualizersLibPaths)
import NodeEditor.State.Global            (State)


loadGraph :: GraphLocation -> Maybe (GraphLocation, LocationSettings) -> Bool -> Command State ()
loadGraph location prevSettings retrieveLocation = do
    visLibPaths <- view visualizersLibPaths <$> getNodeEditor
    resetGraph
    modifyNodeEditor $ visualizersLibPaths .= visLibPaths
    setGraphStatus GraphLoading
    modifyApp $ workspace . _Just . currentLocation .= location
    Atom.setActiveLocation location
    Batch.getProgram prevSettings retrieveLocation

navigateToGraph :: GraphLocation -> Command State ()
navigateToGraph location = do
    mayWorkspace <- getWorkspace
    let mayCurrentLoc = mayWorkspace ^? _Just . currentLocation
    when (mayCurrentLoc /= Just location) $ do
        settings <- if (view filePath <$> mayCurrentLoc) == Just (location ^. filePath)
            then LocationSettings Nothing <$> getScreenTransform
            else getSettings
        loadGraph location ((, settings) <$> mayCurrentLoc) False

getSettings :: Command State LocationSettings
getSettings = LocationSettings <$> (Just . fmap Project.toOldAPI <$> use Global.preferedVisualizers) <*> getScreenTransform

saveSettings :: Command State ()
saveSettings = getSettings >>= Batch.saveSettings
