module Luna.Studio.Action.State.Scene where

import           Luna.Prelude
import           Data.Position                       (Position)
import           Data.ScreenPosition                 (ScreenPosition (ScreenPosition))
import qualified Data.ScreenPosition                 as ScreenPosition
import           Data.Size                           (Size)
import           Data.Vector                         (scalarProduct, vector, x, y)
import           Luna.Studio.Action.Command          (Command)
import qualified JS.Scene                            as Scene
import           Luna.Studio.Action.State.App        (renderIfNeeded)
import           Luna.Studio.Action.State.NodeEditor (getScreenTranform, modifyNodeEditor)
import qualified Luna.Studio.Action.State.NodeEditor as NE
import           Luna.Studio.React.Model.Layout      (Scene)
import qualified Luna.Studio.React.Model.Layout      as Scene
import qualified Luna.Studio.React.Model.NodeEditor  as NodeEditor
import           Luna.Studio.React.Model.Sidebar     (InputSidebar, OutputSidebar)
import qualified Luna.Studio.React.Model.Sidebar     as Sidebar
import           Luna.Studio.State.Global            (State)


translateToWorkspace :: ScreenPosition -> Command State Position
translateToWorkspace pos = Scene.translateToWorkspace pos <$> getScreenTranform

translateToScreen :: Position -> Command State ScreenPosition
translateToScreen pos = Scene.translateToScreen pos <$> getScreenTranform

-- WARNING: Those functions can discretely change our app, be sure to redraw connections for sidebars!

getScene :: Command State (Maybe Scene)
getScene = NE.getScene >>= maybe (updateScene >> NE.getScene) (return . return . id)

updateScene :: Command State ()
updateScene = do
    renderIfNeeded
    mayNewScene <- Scene.get
    let shouldUpdate = flip (maybe True) mayNewScene $ \newScene ->
            newScene ^. Scene.position /= def || newScene ^. Scene.size /= def
    when shouldUpdate $ modifyNodeEditor $ NodeEditor.layout . Scene.scene .= mayNewScene

getWorkspacePosition :: Command State (Maybe ScreenPosition)
getWorkspacePosition = view Scene.position `fmap2` getScene

getScreenSize :: Command State (Maybe Size)
getScreenSize = view Scene.size `fmap2` getScene

getScreenRightCenter :: Command State (Maybe ScreenPosition)
getScreenRightCenter = fmap2 (\s -> ScreenPosition.fromDoubles (s ^. x) (s ^. y / 2)) getScreenSize

getScreenLeftCenter :: Command State (Maybe ScreenPosition)
getScreenLeftCenter = fmap2 (\s -> ScreenPosition.fromDoubles 0 (s ^. y / 2)) getScreenSize

getScreenCenter :: Command State (Maybe ScreenPosition)
getScreenCenter = fmap2 (ScreenPosition . flip scalarProduct 0.5 . view vector) getScreenSize

getInputSidebar :: Command State (Maybe InputSidebar)
getInputSidebar =  join <$> view Scene.inputSidebar `fmap2` getScene

getOutputSidebar :: Command State (Maybe OutputSidebar)
getOutputSidebar = join <$> view Scene.outputSidebar `fmap2` getScene

getInputSidebarSize :: Command State (Maybe Size)
getInputSidebarSize = view Sidebar.inputSidebarSize `fmap2` getInputSidebar
