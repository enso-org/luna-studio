{-# LANGUAGE NamedFieldPuns #-}

module Luna.Studio.Commands.Camera
     ( autoZoom
     , panCamera
     , panDown
     , panDrag
     , panLeft
     , panRight
     , panUp
     , resetCamera
     , resetCameraState
     , resetPan
     , resetZoom
     , startPanDrag
     , startZoomDrag
     , translateToWorkspace
     , wheelZoom
     , zoomDrag
     , zoomIn
     , zoomOut
     ) where

import           Data.Matrix                           (Matrix, getElem, identity, multStd2, setElem)
import           Data.Matrix                           as Matrix
import qualified Empire.API.Data.Node                  as Node
import           Luna.Studio.Commands.Command          (Command)
import           Luna.Studio.Data.CameraTransformation (lastInverse, logicalToScreen, screenToLogical)
import           Luna.Studio.Data.Matrix               (homothetyMatrix, invertedHomothetyMatrix, invertedTranslationMatrix,
                                                        translationMatrix)
import           Luna.Studio.Data.Vector               (Position (Position), ScreenPosition, Size (Size), Vector2 (Vector2), fromTuple,
                                                        minimumRectangle, scalarProduct, vector, x, y)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import qualified Luna.Studio.React.Store               as Store
import qualified Luna.Studio.State.Camera              as Camera
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global
import qualified Luna.Studio.State.Graph               as Graph


minCamFactor, maxCamFactor, dragZoomSpeed, wheelZoomSpeed, panStep, zoomFactorStep :: Double
minCamFactor   = 0.2
maxCamFactor   = 8
dragZoomSpeed  = 512
wheelZoomSpeed = 64
panStep        = 50
zoomFactorStep = 1.1
padding :: Vector2 Double
padding = Vector2 80 80

foreign import javascript safe "document.getElementById('Graph').offsetWidth"  screenWidth  :: IO Double
foreign import javascript safe "document.getElementById('Graph').offsetHeight" screenHeight :: IO Double


getScreenCenter :: Command State Position
getScreenCenter = getScreenCenterFromSize <$> getScreenSize

getScreenCenterFromSize :: Size -> Position
getScreenCenterFromSize = Position . flip scalarProduct 0.5 . view vector

getScreenSize :: Command State Size
getScreenSize = liftIO $ Size . fromTuple <$> ((,) <$> screenWidth <*> screenHeight)

modifyCamera :: Matrix Double -> Matrix Double -> Command State ()
modifyCamera matrix invertedMatrix = Global.withNodeEditor $ Store.modifyM_ $ do
    NodeEditor.screenTransform . logicalToScreen %= (flip multStd2 matrix)
    transformsSinceLastInverse <- use $ NodeEditor.screenTransform . lastInverse
    if transformsSinceLastInverse < 100
    then do
        originalMatrix <- use $ NodeEditor.screenTransform . logicalToScreen
        case inverse originalMatrix of
            Right m -> do
                NodeEditor.screenTransform . screenToLogical .= m
                NodeEditor.screenTransform . lastInverse     .= 0
            _       -> do
                NodeEditor.screenTransform . screenToLogical %= (multStd2 invertedMatrix)
                NodeEditor.screenTransform . lastInverse     += 1
    else do
        NodeEditor.screenTransform . screenToLogical %= (multStd2 invertedMatrix)
        NodeEditor.screenTransform . lastInverse     += 1

resetCamera :: Command State ()
resetCamera = Global.withNodeEditor $ Store.modifyM_ $ do
    NodeEditor.screenTransform . logicalToScreen .= identity 4
    NodeEditor.screenTransform . screenToLogical .= identity 4

resetCameraState :: Command State ()
resetCameraState = Global.cameraState .= Nothing

translateToWorkspace :: Position -> Command State (Position)
translateToWorkspace pos = do
    transformMatrix <- Global.withNodeEditor $ Store.use $ NodeEditor.screenTransform . screenToLogical
    let posMatrix      = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace = multStd2 posMatrix transformMatrix
    return $ Position (Vector2 (getElem 1 1 posInWorkspace) (getElem 1 2 posInWorkspace))



restrictFactor :: Double -> Double -> Double
restrictFactor scale factor
    | scale * factor < minCamFactor = minCamFactor / scale
    | scale * factor > maxCamFactor = maxCamFactor / scale
    | otherwise                     = factor

panCamera :: Vector2 Double -> Command State ()
panCamera delta = modifyCamera (translationMatrix delta) (invertedTranslationMatrix delta)

panLeft, panRight, panUp, panDown :: Command State ()
panLeft  = panCamera $ Vector2 (-panStep) 0
panRight = panCamera $ Vector2 panStep    0
panUp    = panCamera $ Vector2 0          (-panStep)
panDown  = panCamera $ Vector2 0          panStep

startPanDrag :: ScreenPosition -> Command State ()
startPanDrag pos = Global.cameraState ?= Camera.PanDrag pos

panDrag :: ScreenPosition -> Command State ()
panDrag actPos = do
    mayState <- use Global.cameraState
    withJust mayState $ \state -> case state of
        Camera.PanDrag prevPos -> do
            Global.cameraState ?= Camera.PanDrag actPos
            let delta = actPos ^. vector - prevPos ^. vector
            panCamera delta
        _ -> return ()

resetPan :: Command State ()
resetPan = Global.withNodeEditor $ Store.modifyM_ $ do
    NodeEditor.screenTransform . logicalToScreen %= (setElem 0 (4,1) . setElem 0 (4,2))
    NodeEditor.screenTransform . screenToLogical %= (setElem 0 (4,1) . setElem 0 (4,2))



zoomCamera :: ScreenPosition -> Double -> Command State ()
zoomCamera zoomCenter factor = do
    transformMatrix <- Global.withNodeEditor $ Store.use $ NodeEditor.screenTransform . logicalToScreen
    let s = restrictFactor (getElem 1 1 transformMatrix) factor
    modifyCamera (homothetyMatrix zoomCenter s) (invertedHomothetyMatrix zoomCenter s)

zoomIn :: Command State ()
zoomIn = getScreenCenter >>= flip zoomCamera zoomFactorStep

zoomOut :: Command State ()
zoomOut = getScreenCenter >>= flip zoomCamera (1/zoomFactorStep)

startZoomDrag :: ScreenPosition -> Command State ()
startZoomDrag pos = Global.cameraState ?= Camera.ZoomDrag pos pos

zoomDrag :: ScreenPosition -> Command State ()
zoomDrag actPos = do
    mayState <- use Global.cameraState
    withJust mayState $ \state -> case state of
        Camera.ZoomDrag fixedPoint prevPos -> do
            Global.cameraState ?= Camera.ZoomDrag fixedPoint actPos
            let delta = actPos ^. vector - prevPos ^. vector
                scale = 1 + (delta ^. x - delta ^. y) / dragZoomSpeed
            zoomCamera fixedPoint scale
        _ -> return ()

resetZoom :: Command State ()
resetZoom = Global.withNodeEditor $ Store.modifyM_ $ do
    NodeEditor.screenTransform . logicalToScreen %= (setElem 1 (1,1) . setElem 1 (2,2))
    NodeEditor.screenTransform . screenToLogical %= (setElem 1 (1,1) . setElem 1 (2,2))

wheelZoom :: ScreenPosition -> Double -> Command State ()
wheelZoom pos delta = zoomCamera pos (1 + delta / wheelZoomSpeed)



autoZoom :: Command State ()
autoZoom = do
    nodes <- use $ Global.graph . Graph.nodes
    case minimumRectangle $ map (Position . fromTuple) $ view Node.position <$> nodes of
        Just (leftTop, rightBottom) -> do
            screenSize <- getScreenSize
            let screenCenter = getScreenCenterFromSize screenSize
                span         = Size (rightBottom ^. vector - leftTop ^. vector + scalarProduct padding 2)
                shift        = padding + screenCenter ^. vector - scalarProduct (span ^. vector) 0.5 - leftTop ^. vector
                factor       = min 1 $ min (screenSize ^. x / span ^. x) (screenSize ^. y / span ^. y)

            Global.withNodeEditor $ Store.modifyM_ $ do
                NodeEditor.screenTransform . logicalToScreen .= multStd2 (translationMatrix shift) (homothetyMatrix screenCenter factor)
                NodeEditor.screenTransform . screenToLogical .= multStd2 (invertedHomothetyMatrix screenCenter factor) (invertedTranslationMatrix shift)
        Nothing -> resetCamera
