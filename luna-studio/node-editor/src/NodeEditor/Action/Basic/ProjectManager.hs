module NodeEditor.Action.Basic.ProjectManager where

import           Common.Prelude
import qualified JS.Atom                            as Atom
import           LunaStudio.Data.GraphLocation      (GraphLocation, filePath)
import           LunaStudio.Data.Project            (LocationSettings (LocationSettings))
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.NodeEditor (getScreenTranform, resetGraph, setGraphStatus)
import           NodeEditor.Batch.Workspace         (currentLocation)
import           NodeEditor.React.Model.NodeEditor  (GraphStatus (GraphLoading))
import           NodeEditor.State.Global            (State, workspace)
import qualified NodeEditor.State.Global            as Global


loadGraph :: GraphLocation -> Maybe (GraphLocation, LocationSettings) -> Command State ()
loadGraph location prevSettings = do
    resetGraph
    setGraphStatus GraphLoading
    workspace . _Just . currentLocation .= location
    Atom.setActiveLocation location
    Batch.getProgram prevSettings

navigateToGraph :: GraphLocation -> Command State ()
navigateToGraph location = do
    mayCurrentLoc <- preuse $ workspace . traverse . currentLocation
    when (mayCurrentLoc /= Just location) $ do
        settings <- if (view filePath <$> mayCurrentLoc) == Just (location ^. filePath)
            then LocationSettings Nothing <$> getScreenTranform
            else getSettings
        loadGraph location $ (, settings) <$> mayCurrentLoc

getSettings :: Command State LocationSettings
getSettings = LocationSettings <$> (Just <$> use Global.preferedVisualizers) <*> getScreenTranform

saveSettings :: Command State ()
saveSettings = getSettings >>= Batch.saveSettings
