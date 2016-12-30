{-# LANGUAGE NamedFieldPuns #-}

module Luna.Studio.Commands.Camera
     ( panCamera
    --  , panDrag
     , panDown
     , panUp
     , panLeft
     , panRight
     , autoZoom
    --  , syncCamera
    --  , zoomDrag
     , zoomIn
     , zoomOut
    --  , wheelZoom
     , resetCamera
     , resetPan
     , resetZoom
    --  , updateWindowSize --TODO[react] remove
     ) where

import           Data.Matrix                           (Matrix, getElem, identity, multStd2, setElem)

import qualified Empire.API.Data.Node                  as Node
import           Luna.Studio.Commands.Command          (Command)
import           Luna.Studio.Data.CoordsTransformation (logicalToScreen, screenToLogical)
import           Luna.Studio.Data.Matrix               (homothetyMatrix, invertedHomothetyMatrix, invertedScaleMatrix,
                                                        invertedTranslationMatrix, scaleMatrix, translationMatrix)
import           Luna.Studio.Data.Vector               (Position (Position), Size (Size), Vector2 (Vector2), fromTuple, minimumRectangle,
                                                        scalarProduct, vector, x, y)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import qualified Luna.Studio.React.Store               as Store
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global
import qualified Luna.Studio.State.Graph               as Graph

-- import           Reactive.Commands.UILayout as UILayout --TODO[react] remove
-- import           Luna.Studio.State.Camera           (DragHistory (..))
-- import qualified Luna.Studio.State.Camera           as Camera
-- import qualified JS.Camera                             as JS

-- autoZoom :: Command Global.State ()
-- autoZoom = do
--     nodes             <- use $ Global.graph  . Graph.nodes
--     screenSize'       <- use $ Global.camera . Camera.camera . Camera.screenSize
--
--     zoom Global.camera $ setZoom 1.0
--     Global.camera . Camera.camera . Camera.pan    .= Vector2 0.0 0.0
--
--     when (length nodes > 0) $ do
--         let padding        = Vector2 80.0 80.0
--             screenSize     = fromIntegral <$> screenSize'
--             minXY          = -padding + (Vector2 (minimum $ (^. Node.position . _1) <$> nodes) (minimum $ (^. Node.position . _2) <$> nodes))
--             maxXY          =  padding + (Vector2 (maximum $ (^. Node.position . _1) <$> nodes) (maximum $ (^. Node.position . _2) <$> nodes))
--             spanXY         = maxXY - minXY
--             zoomFactorXY   = Vector2 (screenSize ^. x / spanXY ^. x) (screenSize ^. y / spanXY ^. y)
--             zoomFactor     = min 1.0 $ min (zoomFactorXY ^. x) (zoomFactorXY ^. y)
--             zoomPan        = minXY + ((/2.0) <$> spanXY)
--
--         zoom Global.camera $ setZoom zoomFactor
--         Global.camera . Camera.camera . Camera.pan    .= zoomPan
--
--     zoom Global.camera syncCamera

-- wheelZoom :: Position -> Vector2 Double -> Command Camera.State ()
-- wheelZoom pos delta = do
--     camera         <- use $ Camera.camera
--     let delta'      = (- delta ^. x - delta ^. y) / wheelZoomSpeed
--         workspace   = Camera.screenToWorkspace camera pos
--     fixedPointZoom pos workspace delta'
--
-- fixedPointZoom :: Position -> Position -> Double -> Command Camera.State ()
-- fixedPointZoom fpScreen fpWorkspace delta = do
--     oldFactor           <- use $ Camera.camera . Camera.factor
--
--     let newFactor        = oldFactor * (1.0 + delta)
--     setZoom newFactor
--
--     oldCamera           <- use $ Camera.camera
--     let nonPannedCamera  = oldCamera & Camera.factor .~ (restrictCamFactor newFactor)
--                                      & Camera.pan    .~ Vector2 0.0 0.0
--         newWorkspace     = Camera.screenToWorkspace nonPannedCamera fpScreen
--         newPan           = -newWorkspace + fpWorkspace
--
--     Camera.camera . Camera.pan .= newPan
--
-- panDrag :: Mouse.Type -> Vector2 Int -> Command Camera.State ()
-- panDrag Mouse.Pressed pos = do
--     Camera.history ?= PanDragHistory pos
--
-- panDrag Mouse.Moved   pos = do
--     history <- use $ Camera.history
--     case history of
--         Just (PanDragHistory prev) -> do
--             Camera.history ?= PanDragHistory pos
--             panCamera $ fromIntegral <$> prev - pos
--         _                          -> return ()
--
-- panDrag Mouse.Released _ = do
--     Camera.history .= Nothing
--
-- panDrag _ _ = return ()
--
-- zoomDrag :: Mouse.Type -> Vector2 Int -> Command Camera.State ()
-- zoomDrag Mouse.Pressed screenPos = do
--     camera           <- use $ Camera.camera
--     let workspacePos  = Camera.screenToWorkspace camera screenPos
--     Camera.history   ?= ZoomDragHistory screenPos screenPos workspacePos
--
-- zoomDrag Mouse.Moved   pos = do
--     history <- use $ Camera.history
--     case history of
--         Just (ZoomDragHistory prev fpScreen fpWorkspace) -> do
--             Camera.history ?= ZoomDragHistory pos fpScreen fpWorkspace
--             let deltaV = fromIntegral <$> (prev - pos)
--                 delta  = (-deltaV ^. x + deltaV ^. y) / dragZoomSpeed
--             fixedPointZoom fpScreen fpWorkspace delta
--         _                           -> return ()
--
-- zoomDrag Mouse.Released _ = do
--     Camera.history .= Nothing
--
-- zoomDrag _ _ = return ()
--
-- syncCamera :: Command Camera.State ()
-- syncCamera = do
--     cPan            <- use $ Camera.camera . Camera.pan
--     cFactor         <- use $ Camera.camera . Camera.factor
--     screenSize      <- use $ Camera.camera . Camera.screenSize
--     let hScreen      = (/ 2.0) . fromIntegral <$> screenSize
--         camLeft      = appX cameraLeft
--         camRight     = appX cameraRight
--         camTop       = appY cameraTop
--         camBottom    = appY cameraBottom
--         hX           = appX htmlX
--         hY           = appY htmlY
--         appX      f  = f cFactor (cPan ^. x) (hScreen ^. x)
--         appY      f  = f cFactor (cPan ^. y) (hScreen ^. y)
--     performIO $ do
--         JS.updateCamera cFactor camLeft camRight camTop camBottom
--         JS.updateCameraHUD 0.0 (fromIntegral $ screenSize ^. x) 0.0 (fromIntegral $ screenSize ^. y)
--         JS.updateHtmCanvasPanPos hX hY cFactor
--         JS.updateProjectionMatrix
--         JS.updateHUDProjectionMatrix
--
--
--
-- cameraLeft, cameraRight, cameraTop, cameraBottom, htmlX, htmlY :: Double -> Double -> Double -> Double
-- cameraLeft   camFactor camPanX halfScreenX = -halfScreenX / camFactor + camPanX
-- cameraRight  camFactor camPanX halfScreenX =  halfScreenX / camFactor + camPanX
-- cameraTop    camFactor camPanY halfScreenY = -halfScreenY / camFactor + camPanY
-- cameraBottom camFactor camPanY halfScreenY =  halfScreenY / camFactor + camPanY
-- htmlX        camFactor camPanX halfScreenX =  halfScreenX - camPanX * camFactor
-- htmlY        camFactor camPanY halfScreenY =  halfScreenY - camPanY * camFactor

--TODO[react] remove
-- updateWindowSize :: Vector2 Int -> Command Global.State ()
-- updateWindowSize size = do
--     textEditorWidth <- UILayout.relayoutTextEditor size
--     zoom Global.camera $ do
--         let canvasWidth = size ^. x - textEditorWidth
--         Camera.camera . Camera.windowSize .= size
--         Camera.camera . Camera.screenSize .= Vector2 canvasWidth (size ^. y)
--         syncCamera
--         performIO $ JS.updateScreenSize canvasWidth (size ^. y)
--     UILayout.relayout

minCamFactor, maxCamFactor, dragZoomSpeed, wheelZoomSpeed, panStep, zoomFactorStep, padding :: Double
minCamFactor   = 0.2
maxCamFactor   = 8
dragZoomSpeed  = 512
wheelZoomSpeed = 64
panStep        = 50
zoomFactorStep = 1.1
padding = 80
--TODO[react]: Find out how to get real screen size
screenSize :: Size
-- screenSize = Size (Vector2 2000 1680)
screenSize = Size (Vector2 1266.66 1191.25)

restrictFactor :: Double -> Double -> Double
restrictFactor scale factor
    | scale * factor < minCamFactor = minCamFactor / scale
    | scale * factor > maxCamFactor = maxCamFactor / scale
    | otherwise                     = factor

--TODO[react]: Take care of numeric stability (always inverse or once in x times)
modifyCamera :: Matrix Double -> Matrix Double -> Command State ()
modifyCamera matrix invertedMatrix = Global.withNodeEditor $ Store.modifyM_ $ do
    NodeEditor.screenTransform . logicalToScreen %= (flip multStd2 matrix)
    NodeEditor.screenTransform . screenToLogical %= (multStd2 invertedMatrix)

panCamera :: Vector2 Double -> Command State ()
panCamera delta = modifyCamera (translationMatrix delta) (invertedTranslationMatrix delta)

panLeft, panRight, panUp, panDown :: Command State ()
panLeft  = panCamera $ Vector2 (-panStep) 0
panRight = panCamera $ Vector2 panStep    0
panUp    = panCamera $ Vector2 0          (-panStep)
panDown  = panCamera $ Vector2 0          panStep

zoomCamera :: Double -> Command State ()
zoomCamera factor = do
    transformMatrix <- Global.withNodeEditor $ Store.use $ NodeEditor.screenTransform . logicalToScreen
    let s              = restrictFactor (getElem 1 1 transformMatrix) factor
    modifyCamera (scaleMatrix s) (invertedScaleMatrix s)

--TODO[react]: Perform zoom relative to the center of screen
zoomIn :: Command State ()
zoomIn = zoomCamera zoomFactorStep

--TODO[react]: Perform zoom relative to the center of screen
zoomOut :: Command State ()
zoomOut = zoomCamera (1/zoomFactorStep)

resetZoom :: Command State ()
resetZoom = Global.withNodeEditor $ Store.modifyM_ $ do
    NodeEditor.screenTransform . logicalToScreen %= (setElem 1 (1,1) . setElem 1 (2,2))
    NodeEditor.screenTransform . screenToLogical %= (setElem 1 (1,1) . setElem 1 (2,2))

resetPan :: Command State ()
resetPan = Global.withNodeEditor $ Store.modifyM_ $ do
    NodeEditor.screenTransform . logicalToScreen %= (setElem 0 (4,1) . setElem 0 (4,2))
    NodeEditor.screenTransform . screenToLogical %= (setElem 0 (4,1) . setElem 0 (4,2))

resetCamera :: Command State ()
resetCamera = Global.withNodeEditor $ Store.modifyM_ $ do
    NodeEditor.screenTransform . logicalToScreen .= identity 4
    NodeEditor.screenTransform . screenToLogical .= identity 4

autoZoom :: Command State ()
autoZoom = do
    nodes <- use $ Global.graph . Graph.nodes
    case minimumRectangle $ map (Position . fromTuple) $ view Node.position <$> nodes of
        Just (leftTop, rightBottom) -> do
            let span         = Size (Vector2 (rightBottom ^. x - leftTop ^. x + 2 * padding) (rightBottom ^. y - leftTop ^. y + 2 * padding))
                shift        = (Vector2 padding padding) + scalarProduct (screenSize ^. vector - span ^. vector) 0.5 - leftTop ^. vector
                factor       = min 1 $ min (screenSize ^. x / span ^. x) (screenSize ^. y / span ^. y)
                screenCenter = Position (scalarProduct (screenSize ^. vector) 0.5)
            Global.withNodeEditor $ Store.modifyM_ $ do
                NodeEditor.screenTransform . logicalToScreen .= multStd2 (translationMatrix shift) (homothetyMatrix screenCenter factor)
                NodeEditor.screenTransform . screenToLogical .= multStd2 (invertedHomothetyMatrix screenCenter factor) (invertedTranslationMatrix shift)
        Nothing -> resetCamera
