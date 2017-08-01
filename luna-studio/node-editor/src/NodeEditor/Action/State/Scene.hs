module NodeEditor.Action.State.Scene where

import           Common.Action.Command              (Command)
import           Common.Prelude
import qualified JS.Scene                           as Scene
import           LunaStudio.Data.Position           (Position)
import           LunaStudio.Data.ScreenPosition     (ScreenPosition)
import qualified LunaStudio.Data.ScreenPosition     as ScreenPosition
import           LunaStudio.Data.Size               (Size)
import           LunaStudio.Data.Vector2            (x, y)
import           NodeEditor.Action.State.App        (renderIfNeeded)
import           NodeEditor.Action.State.NodeEditor (getScreenTransform, modifyNodeEditor)
import qualified NodeEditor.Action.State.NodeEditor as NE
import           NodeEditor.React.Model.Layout      (Scene, screenCenter)
import qualified NodeEditor.React.Model.Layout      as Scene
import qualified NodeEditor.React.Model.NodeEditor  as NodeEditor
import           NodeEditor.React.Model.Sidebar     (InputSidebar, OutputSidebar)
import qualified NodeEditor.React.Model.Sidebar     as Sidebar
import           NodeEditor.State.Global            (State)


translateToWorkspace :: ScreenPosition -> Command State Position
translateToWorkspace pos = Scene.translateToWorkspace pos <$> getScreenTransform <*> getScreenCenter

translateToScreen :: Position -> Command State ScreenPosition
translateToScreen pos = Scene.translateToScreen pos <$> getScreenTransform

getScene :: Command State (Maybe Scene)
getScene = NE.getScene >>= maybe (updateScene >> NE.getScene) (return . return . id)

updateScene :: Command State ()
updateScene = do
    renderIfNeeded
    mayNewScene <- Scene.get
    let shouldUpdate = flip (maybe True) mayNewScene $ \newScene ->
            newScene ^. Scene.position /= def || newScene ^. Scene.size /= def
    when shouldUpdate $
        modifyNodeEditor $ NodeEditor.layout . Scene.scene .= mayNewScene

getWorkspacePosition :: Command State (Maybe ScreenPosition)
getWorkspacePosition = view Scene.position `fmap2` getScene

getScreenSize :: Command State (Maybe Size)
getScreenSize = view Scene.size `fmap2` getScene

getScreenRightCenter :: Command State (Maybe ScreenPosition)
getScreenRightCenter = fmap2 (\s -> ScreenPosition.fromDoubles (s ^. x) (s ^. y / 2)) getScreenSize

getScreenLeftCenter :: Command State (Maybe ScreenPosition)
getScreenLeftCenter = fmap2 (\s -> ScreenPosition.fromDoubles 0 (s ^. y / 2)) getScreenSize

getScreenCenter :: Command State (Maybe ScreenPosition)
getScreenCenter = fmap2 (view screenCenter) getScene

getInputSidebar :: Command State (Maybe InputSidebar)
getInputSidebar =  join <$> view Scene.inputSidebar `fmap2` getScene

getOutputSidebar :: Command State (Maybe OutputSidebar)
getOutputSidebar = join <$> view Scene.outputSidebar `fmap2` getScene

getInputSidebarSize :: Command State (Maybe Size)
getInputSidebarSize = view Sidebar.inputSidebarSize `fmap2` getInputSidebar
