{-# LANGUAGE NamedFieldPuns #-}

module Luna.Studio.Commands.Camera
     ( panCamera
    --  , panDrag
     , panDown
     , panUp
     , panLeft
     , panRight
    --  , autoZoom
    --  , syncCamera
    --  , zoomDrag
     , zoomIn
     , zoomOut
    --  , wheelZoom
     , resetZoom
    --  , updateWindowSize --TODO[react] remove
     ) where

import qualified Empire.API.Data.Node               as Node
import qualified Event.Mouse                        as Mouse
import qualified JS.Camera                          as JS
import           Luna.Studio.Commands.Command       (Command, performIO)
import           Luna.Studio.Data.Vector            (Position, Vector2 (Vector2))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
-- import           Luna.Studio.State.Camera           (DragHistory (..))
-- import qualified Luna.Studio.State.Camera           as Camera
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph
-- import           Reactive.Commands.UILayout as UILayout --TODO[react] remove


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

-- zoomIn :: Command Camera.State ()
-- zoomIn = do
--     factor <- use $ Camera.camera . Camera.factor
--     setZoom $ factor * zoomFactorStep

-- zoomOut :: Command Camera.State ()
-- zoomOut = do
--     factor <- use $ Camera.camera . Camera.factor
--     setZoom $ factor / zoomFactorStep
--
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

minCamFactor, maxCamFactor, dragZoomSpeed, wheelZoomSpeed, panStep, zoomFactorStep :: Double
minCamFactor   =   0.2
maxCamFactor   =   8.0
dragZoomSpeed  = 512.0
wheelZoomSpeed =  64.0
panStep        =  50.0
zoomFactorStep =   1.1

restrictCamFactor :: Double -> Double
restrictCamFactor = min maxCamFactor . max minCamFactor

panCamera :: Vector2 Double -> Command State ()
panCamera delta = Global.withNodeEditor $ Store.modifyM_ $ do
    factor <- use NodeEditor.factor
    NodeEditor.pan += ((/ factor) <$> delta)

panLeft, panRight, panUp, panDown :: Command State ()
panLeft  = panCamera $ Vector2 (-panStep) 0
panRight = panCamera $ Vector2 panStep    0
panUp    = panCamera $ Vector2 0          (-panStep)
panDown  = panCamera $ Vector2 0          panStep

zoomIn :: Command State ()
zoomIn = Global.withNodeEditor $ Store.modifyM_ $ do
    prevFactor <- use NodeEditor.factor
    NodeEditor.factor .= restrictCamFactor (prevFactor * zoomFactorStep)

zoomOut :: Command State ()
zoomOut = Global.withNodeEditor $ Store.modifyM_ $ do
    prevFactor <- use NodeEditor.factor
    NodeEditor.factor .= restrictCamFactor (prevFactor / zoomFactorStep)

resetZoom :: Command State ()
resetZoom = Global.withNodeEditor $ Store.modifyM_ $ do
    NodeEditor.factor .= 1
