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
import           Data.ScreenPosition                   (ScreenPosition, vector, x, y)
import           Data.Vector                           (Vector2)
import           Luna.Studio.Action.Basic              (modifyCamera)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.State.Action       (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                        updateActionWithKey)
import           Luna.Studio.Action.State.NodeEditor   (getNodeEditor, modifyNodeEditor)
import           Luna.Studio.Action.State.Scene        (getScreenCenter)
import           Luna.Studio.Data.CameraTransformation (logicalToScreen, screenToLogical)
import           Luna.Studio.Data.Matrix               (homothetyMatrix, invertedHomothetyMatrix)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.NodeEditor    (screenTransform)
import           Luna.Studio.State.Action              (Action (begin, continue, end, update), ZoomDrag (ZoomDrag), zoomDragAction,
                                                        zoomDragFixedPoint, zoomDragPreviousPos)
import           Luna.Studio.State.Global              (State)


instance Action (Command State) ZoomDrag where
    begin    = beginActionWithKey    zoomDragAction
    continue = continueActionWithKey zoomDragAction
    update   = updateActionWithKey   zoomDragAction
    end _    = removeActionFromState zoomDragAction

minCamFactor, maxCamFactor, dragZoomSpeed, wheelZoomSpeed, zoomFactorStep :: Double
minCamFactor   = 0.26
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
    transformMatrix <- view (screenTransform . logicalToScreen) <$> getNodeEditor
    let s = restrictFactor (getElem 1 1 transformMatrix) factor
    modifyCamera (homothetyMatrix zoomCenter s) (invertedHomothetyMatrix zoomCenter s)

zoomIn :: Command State ()
zoomIn = getScreenCenter >>= \mayCenter -> (withJust mayCenter $ flip zoomCamera zoomFactorStep)

zoomOut :: Command State ()
zoomOut = getScreenCenter >>= \mayCenter -> (withJust mayCenter $ flip zoomCamera (1/zoomFactorStep))

startZoomDrag :: ScreenPosition -> Command State ()
startZoomDrag pos = begin $ ZoomDrag pos pos

zoomDrag :: ScreenPosition -> ZoomDrag -> Command State ()
zoomDrag actPos action = do
    let fixedPoint = action ^. zoomDragFixedPoint
        prevPos    = action ^. zoomDragPreviousPos
        delta      = actPos ^. vector - prevPos ^. vector
        scale      = 1 + (delta ^. x - delta ^. y) / dragZoomSpeed
    update $ ZoomDrag fixedPoint actPos
    zoomCamera fixedPoint scale

resetZoom :: Command State ()
resetZoom = modifyNodeEditor $ do
    screenTransform . logicalToScreen %= (setElem 1 (1,1) . setElem 1 (2,2))
    screenTransform . screenToLogical %= (setElem 1 (1,1) . setElem 1 (2,2))

wheelZoom :: ScreenPosition -> Vector2 Double -> Command State ()
wheelZoom pos delta = zoomCamera pos delta' where
    delta' = 1 + (delta ^. x + delta ^. y) / wheelZoomSpeed

stopZoomDrag :: ZoomDrag -> Command State ()
stopZoomDrag _ = removeActionFromState zoomDragAction
