module Luna.Studio.Handler.Camera
    ( handle
    ) where

import           Data.Vector                        (Vector2 (Vector2))
import           React.Flux                         (MouseEvent, wheelDeltaX, wheelDeltaY)

import           Luna.Studio.Action.Camera          (centerGraph, panCamera, panDown, panDrag, panLeft, panRight, panUp, resetCamera,
                                                     resetPan, resetZoom, startPanDrag, startZoomDrag, stopPanDrag, stopZoomDrag, wheelZoom,
                                                     zoomDrag, zoomIn, zoomOut)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Event.Event            (Event (Shortcut, UI))
import           Luna.Studio.Event.Mouse            (mousePosition)
import qualified Luna.Studio.Event.Mouse            as Mouse
import           Luna.Studio.Event.Shortcut         (ShortcutEvent (..))
import           Luna.Studio.Event.UI               (UIEvent (AppEvent, NodeEditorEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (Action (continue))
import           Luna.Studio.State.Global           (State)



-- TODO[react]: Consider mac trackpad!!!
handle :: Event -> Maybe (Command State ())
handle (Shortcut shortcut)                             = Just $ handleShortcut shortcut
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


handleShortcut :: ShortcutEvent -> Command State ()
handleShortcut = \case
    CenterGraph -> centerGraph
    PanDown     -> panDown
    PanLeft     -> panLeft
    PanRight    -> panRight
    PanUp       -> panUp
    ResetCamera -> resetCamera
    ResetPan    -> resetPan >> centerGraph
    ResetZoom   -> resetZoom
    ZoomIn      -> zoomIn
    ZoomOut     -> zoomOut
    _           -> return ()


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
