module Luna.Studio.Handler.Camera
    ( handle
    ) where

import           Data.Vector                        (Vector2 (Vector2))
import           Luna.Studio.Action.Camera          (centerGraph, panCamera, panDown, panDrag, panLeft, panRight, panUp, resetCamera,
                                                     resetPan, resetZoom, startPanDrag, startZoomDrag, stopPanDrag, stopZoomDrag, wheelZoom,
                                                     zoomDrag, zoomIn, zoomOut)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Event.Event            (Event (Shortcut, UI))
import           Luna.Studio.Event.Mouse            (mousePosition)
import qualified Luna.Studio.Event.Mouse            as Mouse
import qualified Luna.Studio.Event.Shortcut         as Shortcut
import           Luna.Studio.Event.UI               (UIEvent (AppEvent, NodeEditorEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (Action (continue))
import           Luna.Studio.State.Global           (State)
import           React.Flux                         (MouseEvent, wheelDeltaX, wheelDeltaY)



-- TODO[react]: Consider mac trackpad!!!
handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event command _))           = Just $ handleCommand command
handle (UI (NodeEditorEvent (NodeEditor.MouseDown e))) = Just $ handleMouseDown e
handle (UI (AppEvent (App.MouseMove e _)))             = Just $ handleMouseMove e
handle (UI (AppEvent (App.MouseUp   _)))               = Just $ continue stopPanDrag >> continue stopZoomDrag
handle (UI (NodeEditorEvent (NodeEditor.Wheel m w)))   = Just $ handleMouseWheel m delta where
    deltaX = fromIntegral $ -(wheelDeltaX w)
    deltaY = fromIntegral $ -(wheelDeltaY w)
    delta  = Vector2 deltaX deltaY
handle _                                               = Nothing


-- TODO consider using state and below approach
-- init = do
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--


handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.CenterGraph -> centerGraph
    Shortcut.PanDown     -> panDown
    Shortcut.PanLeft     -> panLeft
    Shortcut.PanRight    -> panRight
    Shortcut.PanUp       -> panUp
    Shortcut.ResetCamera -> resetCamera
    Shortcut.ResetPan    -> resetPan >> centerGraph
    Shortcut.ResetZoom   -> resetZoom
    Shortcut.ZoomIn      -> zoomIn
    Shortcut.ZoomOut     -> zoomOut
    _                    -> return ()


handleMouseDown :: MouseEvent -> Command State ()
handleMouseDown evt
    | Mouse.withoutMods evt Mouse.rightButton  = startZoomDrag =<< mousePosition evt
    | Mouse.withoutMods evt Mouse.middleButton = startPanDrag  =<< mousePosition evt
    | otherwise                                = return ()

handleMouseMove :: MouseEvent -> Command State ()
handleMouseMove evt
    | Mouse.withoutMods evt Mouse.rightButton  = continue . zoomDrag =<< mousePosition evt
    | Mouse.withoutMods evt Mouse.middleButton = continue . panDrag  =<< mousePosition evt
    | otherwise                                = return ()

handleMouseWheel :: MouseEvent -> Vector2 Double -> Command State ()
handleMouseWheel evt delta
    | Mouse.withoutMods evt Mouse.leftButton = panCamera delta
    | Mouse.withCtrl    evt Mouse.leftButton = flip wheelZoom delta =<< mousePosition evt
    | otherwise                            = return ()
