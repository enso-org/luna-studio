{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Camera.Zoom
     ( resetZoom
     , resetZoomState
     , zoomIn
     , zoomOut
     , startZoomDrag
     , zoomDrag
     , wheelZoom
     ) where

import           Data.Matrix                           (getElem, setElem)
import           Luna.Studio.Action.Camera.Modify      (modifyCamera)
import           Luna.Studio.Action.Camera.Screen      (getScreenCenter)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Data.CameraTransformation (logicalToScreen, screenToLogical)
import           Luna.Studio.Data.Matrix               (homothetyMatrix, invertedHomothetyMatrix)
import           Luna.Studio.Data.Vector               (ScreenPosition, Vector2, vector, x, y)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.State.Action              (Action (ZoomDrag))
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global
import           Luna.Studio.State.StatefulAction      (StatefulAction (continue, exit, matchState, pack, start, update))
import qualified Luna.Studio.State.ZoomDrag            as ZoomDrag


instance StatefulAction ZoomDrag.State where
    matchState (ZoomDrag state) = Just state
    matchState _ = Nothing
    pack = ZoomDrag
    exit = resetZoomState

minCamFactor, maxCamFactor, dragZoomSpeed, wheelZoomSpeed, zoomFactorStep :: Double
minCamFactor   = 0.2
maxCamFactor   = 8
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
zoomIn = getScreenCenter >>= flip zoomCamera zoomFactorStep

zoomOut :: Command State ()
zoomOut = getScreenCenter >>= flip zoomCamera (1/zoomFactorStep)

startZoomDrag :: ScreenPosition -> Command State ()
startZoomDrag pos = start $ ZoomDrag.State pos pos

zoomDrag :: ScreenPosition -> ZoomDrag.State -> Command State ()
zoomDrag actPos state = do
    let fixedPoint = view ZoomDrag.fixedPoint state
        prevPos    = view ZoomDrag.previousPos state
        delta = actPos ^. vector - prevPos ^. vector
        scale = 1 + (delta ^. x - delta ^. y) / dragZoomSpeed
    update $ ZoomDrag.State fixedPoint actPos
    zoomCamera fixedPoint scale

resetZoom :: Command State ()
resetZoom = Global.modifyNodeEditor $ do
    NodeEditor.screenTransform . logicalToScreen %= (setElem 1 (1,1) . setElem 1 (2,2))
    NodeEditor.screenTransform . screenToLogical %= (setElem 1 (1,1) . setElem 1 (2,2))

wheelZoom :: ScreenPosition -> Vector2 Double -> Command State ()
wheelZoom pos delta = zoomCamera pos delta' where
    delta' = 1 + (delta ^. x + delta ^. y) / wheelZoomSpeed

resetZoomState :: ZoomDrag.State -> Command State ()
resetZoomState _ = Global.performedAction .= Nothing
