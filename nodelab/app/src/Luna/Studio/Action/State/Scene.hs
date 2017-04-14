module Luna.Studio.Action.State.Scene where

import           Data.Matrix                           (getElem, multStd2)
import qualified Data.Matrix                           as Matrix
import           Data.Position                         (Position)
import qualified Data.Position                         as Position
import           Data.ScreenPosition                   (ScreenPosition (ScreenPosition))
import qualified Data.ScreenPosition                   as ScreenPosition
import           Data.Size                             (Size)
import           Data.Vector                           (scalarProduct, vector, x, y)
import           JS.Scene                              (InputSidebar, OutputSidebar, Scene)
import qualified JS.Scene                              as Scene
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.State.App          (renderIfNeeded)
import           Luna.Studio.Action.State.NodeEditor   (getNodeEditor, modifyNodeEditor)
import qualified Luna.Studio.Action.State.NodeEditor   as NE
import           Luna.Studio.Data.CameraTransformation (logicalToScreen, screenToLogical)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.State.Global              (State)


translateToWorkspace :: ScreenPosition -> Command State Position
translateToWorkspace pos = do
    transformMatrix <- view (NodeEditor.screenTransform . screenToLogical) <$> getNodeEditor
    let posMatrix      = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace = multStd2 posMatrix transformMatrix
    return $ Position.fromDoubles (getElem 1 1 posInWorkspace) (getElem 1 2 posInWorkspace)

translateToScreen :: Position -> Command State ScreenPosition
translateToScreen pos = do
    transformMatrix <- view (NodeEditor.screenTransform . logicalToScreen) <$> getNodeEditor
    let posMatrix      = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace = multStd2 posMatrix transformMatrix
    return $ ScreenPosition.fromDoubles (getElem 1 1 posInWorkspace) (getElem 1 2 posInWorkspace)


-- WARNING: Those functions can discretely change our app, be sure to redraw connections for sidebars!

getScene :: Command State (Maybe Scene)
getScene = NE.getScene >>= maybe (updateScene >> NE.getScene) (return . return . id)

updateScene :: Command State ()
updateScene = do
    renderIfNeeded
    mayNewScene <- Scene.get
    let shouldUpdate = flip (maybe True) mayNewScene $ \newScene ->
            newScene ^. Scene.position /= def || newScene ^. Scene.size /= def
    when shouldUpdate $ modifyNodeEditor $ NodeEditor.scene .= mayNewScene

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
getInputSidebar =  getInputSidebar' >>= maybe (updateScene >> getInputSidebar') (return . return . id) where
    getInputSidebar' = maybe Nothing (view Scene.inputSidebar) <$> NE.getScene

getOutputSidebar :: Command State (Maybe OutputSidebar)
getOutputSidebar =  getOutputSidebar' >>= maybe (updateScene >> getOutputSidebar') (return . return . id) where
    getOutputSidebar' = maybe Nothing (view Scene.outputSidebar) <$> NE.getScene

getInputSidebarPosition :: Command State (Maybe ScreenPosition)
getInputSidebarPosition = view Scene.inputSidebarPosition `fmap2` getInputSidebar

getInputSidebarSize :: Command State (Maybe Size)
getInputSidebarSize = view Scene.inputSidebarSize `fmap2` getInputSidebar

getOutputSidebarPosition :: Command State (Maybe ScreenPosition)
getOutputSidebarPosition = view Scene.outputSidebarPosition `fmap2` getOutputSidebar

getOutputSidebarSize :: Command State (Maybe Size)
getOutputSidebarSize = view Scene.outputSidebarSize `fmap2` getOutputSidebar
