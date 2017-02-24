module Luna.Studio.Action.Camera.Screen
     ( getScreenCenter
     , getScreenCenterFromSize
     , getScreenSize
     , getWorkspacePos
     , translateToScreen
     , translateToWorkspace
     , getScreenLeftCenter
     , getScreenRightCenter
     , getInputSidebar
     , getInputSidebarPosition
     , getInputSidebarSize
     , getOutputSidebar
     , getOutputSidebarPosition
     , getOutputSidebarSize
     , updateSceneOnly
     ) where

import           Data.Matrix                           (getElem, multStd2)
import qualified Data.Matrix                           as Matrix
import           Data.Position                         (Position (Position))
import           Data.ScreenPosition                   (ScreenPosition (ScreenPosition))
import           Data.Size                             (Size)
import           Data.Vector                           (Vector2 (Vector2), fromTuple, scalarProduct, vector, x, y)
import qualified JS.Scene                              as Scene
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Data.CameraTransformation (logicalToScreen, screenToLogical)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global


getScene :: Command State (Maybe Scene.Scene)
getScene = use Global.scene >>= \scene -> if isJust scene then
        return scene
    else updateSceneOnly >> use Global.scene

getWorkspacePos :: Command State (Maybe ScreenPosition)
getWorkspacePos = (fmap . fmap) (view Scene.position) getScene

getScreenCenter :: Command State (Maybe ScreenPosition)
getScreenCenter = (fmap . fmap) getScreenCenterFromSize getScreenSize

getScreenCenterFromSize :: Size -> ScreenPosition
getScreenCenterFromSize = ScreenPosition . flip scalarProduct 0.5 . view vector

getScreenLeftCenter :: Command State (Maybe ScreenPosition)
getScreenLeftCenter = (fmap . fmap) getScreenLeftCenterFromSize getScreenSize

getScreenLeftCenterFromSize :: Size -> ScreenPosition
getScreenLeftCenterFromSize s = ScreenPosition (Vector2 0 (s ^. y / 2))

getScreenRightCenter :: Command State (Maybe ScreenPosition)
getScreenRightCenter = (fmap . fmap) getScreenRightCenterFromSize getScreenSize

getScreenRightCenterFromSize :: Size -> ScreenPosition
getScreenRightCenterFromSize s = ScreenPosition (Vector2 (s ^. x) (s ^. y / 2))

getScreenSize :: Command State (Maybe Size)
getScreenSize = (fmap . fmap) (view Scene.size) $ getScene

getInputSidebar' :: Command State (Maybe Scene.InputSidebar)
getInputSidebar' = fmap join $ (fmap . fmap) (view Scene.inputSidebar) $ use $ Global.scene

getInputSidebar :: Command State (Maybe Scene.InputSidebar)
getInputSidebar = do
    mayInputSidebar <- getInputSidebar'
    if isJust mayInputSidebar then
            return mayInputSidebar
        else updateSceneOnly >> getInputSidebar'

getInputSidebarPosition :: Command State (Maybe ScreenPosition)
getInputSidebarPosition = (fmap . fmap) (view Scene.inputSidebarPosition) getInputSidebar

getInputSidebarSize :: Command State (Maybe Size)
getInputSidebarSize = (fmap . fmap) (view Scene.inputSidebarSize) getInputSidebar

getOutputSidebar' :: Command State (Maybe Scene.OutputSidebar)
getOutputSidebar' = fmap join $ (fmap . fmap) (view Scene.outputSidebar) $ use $ Global.scene

getOutputSidebar :: Command State (Maybe Scene.OutputSidebar)
getOutputSidebar = do
    mayOutputSidebar <- getOutputSidebar'
    if isJust mayOutputSidebar then
            return mayOutputSidebar
        else updateSceneOnly >> getOutputSidebar'

getOutputSidebarPosition :: Command State (Maybe ScreenPosition)
getOutputSidebarPosition = (fmap . fmap) (view Scene.outputSidebarPosition) getOutputSidebar

getOutputSidebarSize :: Command State (Maybe Size)
getOutputSidebarSize = (fmap . fmap) (view Scene.outputSidebarSize) getOutputSidebar

translateToWorkspace :: ScreenPosition -> Command State Position
translateToWorkspace pos = do
    transformMatrix <- view (NodeEditor.screenTransform . screenToLogical) <$> Global.getNodeEditor
    let posMatrix      = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace = multStd2 posMatrix transformMatrix
    return $ Position $ fromTuple (getElem 1 1 posInWorkspace, getElem 1 2 posInWorkspace)

translateToScreen :: Position -> Command State ScreenPosition
translateToScreen pos = do
    transformMatrix <- view (NodeEditor.screenTransform . logicalToScreen) <$> Global.getNodeEditor
    let posMatrix      = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace = multStd2 posMatrix transformMatrix
    return $ ScreenPosition $ fromTuple (getElem 1 1 posInWorkspace, getElem 1 2 posInWorkspace)

updateSceneOnly :: Command State ()
updateSceneOnly = do
    mayNewScene <- Scene.get
    let shouldUpdate = case mayNewScene of
            Nothing -> True
            Just newScene -> newScene ^. Scene.position /= def || newScene ^. Scene.size /= def
    when shouldUpdate $ Global.scene .= mayNewScene
