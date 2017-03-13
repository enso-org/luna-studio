{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Camera.Zoom
     ( resetZoom
     , stopZoomDrag
     , zoomIn
     , zoomOut
     , startZoomDrag
     , zoomDrag
     , wheelZoom
     ) where

import           Data.Matrix                           (getElem, setElem)
import           Data.ScreenPosition                   (ScreenPosition, Vector2, vector, x, y)
import           Luna.Studio.Action.Camera.Modify      (modifyCamera)
import           Luna.Studio.Action.Camera.Screen      (getScreenCenter)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Data.CameraTransformation (logicalToScreen, screenToLogical)
import           Luna.Studio.Data.Matrix               (homothetyMatrix, invertedHomothetyMatrix)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.State.Action              (Action (begin, continue, end, update), ZoomDrag (ZoomDrag), zoomDragAction)
import qualified Luna.Studio.State.Action              as Action
import           Luna.Studio.State.Global              (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                        updateActionWithKey)
import qualified Luna.Studio.State.Global              as Global


instance Action (Command State) ZoomDrag where
    begin    = beginActionWithKey    zoomDragAction
    continue = continueActionWithKey zoomDragAction
    update   = updateActionWithKey   zoomDragAction
    end _    = removeActionFromState zoomDragAction

minCamFactor, maxCamFactor, dragZoomSpeed, wheelZoomSpeed, zoomFactorStep :: Double
minCamFactor   = 0.25
maxCamFactor   = 1.2
dragZoomSpeed  = 512
wheelZoomSpeed = 64
zoomFactorStep = 1.1

restrictFactor :: Double -> Double -> Double
restrictFactor scale factor
    | scale * factor < minCamFactor = minCamFactor / scale
    | scale * factor > maxCamFactor = maxCamFactor / scale
    | otherwise                     = factor

zoomCamera :: ScreenPosition -> Double -> Command State ()
zoomCamera zoomCenter factor = do
    transformMatrix <- view (NodeEditor.screenTransform . logicalToScreen) <$> Global.getNodeEditor
    let s = restrictFactor (getElem 1 1 transformMatrix) factor
    modifyCamera (homothetyMatrix zoomCenter s) (invertedHomothetyMatrix zoomCenter s)

zoomIn :: Command State ()
zoomIn = getScreenCenter >>= \mayCenter -> (withJust mayCenter $ flip zoomCamera zoomFactorStep)

zoomOut :: Command State ()
zoomOut = getScreenCenter >>= \mayCenter -> (withJust mayCenter $ flip zoomCamera (1/zoomFactorStep))

startZoomDrag :: ScreenPosition -> Command State ()
startZoomDrag pos = begin $ ZoomDrag pos pos

zoomDrag :: ScreenPosition -> ZoomDrag -> Command State ()
zoomDrag actPos state = do
    let fixedPoint = view Action.zoomDragFixedPoint  state
        prevPos    = view Action.zoomDragPreviousPos state
        delta      = actPos ^. vector - prevPos ^. vector
        scale      = 1 + (delta ^. x - delta ^. y) / dragZoomSpeed
    update $ ZoomDrag fixedPoint actPos
    zoomCamera fixedPoint scale

resetZoom :: Command State ()
resetZoom = Global.modifyNodeEditor $ do
    NodeEditor.screenTransform . logicalToScreen %= (setElem 1 (1,1) . setElem 1 (2,2))
    NodeEditor.screenTransform . screenToLogical %= (setElem 1 (1,1) . setElem 1 (2,2))

wheelZoom :: ScreenPosition -> Vector2 Double -> Command State ()
wheelZoom pos delta = zoomCamera pos delta' where
    delta' = 1 + (delta ^. x + delta ^. y) / wheelZoomSpeed

stopZoomDrag :: ZoomDrag -> Command State ()
stopZoomDrag _ = removeActionFromState zoomDragAction
