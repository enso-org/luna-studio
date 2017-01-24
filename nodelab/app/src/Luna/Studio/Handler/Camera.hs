module Luna.Studio.Handler.Camera
    ( toAction
    ) where

import           Data.Vector                        (Vector2 (Vector2))
import           Luna.Studio.Action.Camera          (centerGraph, panCamera, panDown, panDrag, panLeft, panRight, panUp, resetCamera,
                                                     resetPan, resetZoom, startPanDrag, startZoomDrag, stopPanDrag, stopZoomDrag, wheelZoom,
                                                     zoomDrag, zoomIn, zoomOut)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Event.Event            (Event (UI))
import qualified Luna.Studio.Event.Keys             as Keys
import           Luna.Studio.Event.Mouse            (mousePosition)
import qualified Luna.Studio.Event.Mouse            as Mouse
import           Luna.Studio.Event.UI               (UIEvent (AppEvent, NodeEditorEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (Action (continue))
import           Luna.Studio.State.Global           (State)
import           React.Flux                         (KeyboardEvent, MouseEvent, wheelDeltaX, wheelDeltaY)


-- TODO[react]: Consider mac trackpad!!!
toAction :: Event -> Maybe (Command State ())
toAction (UI (AppEvent (App.KeyDown   e)))               = Just $ handleKey e
toAction (UI (NodeEditorEvent (NodeEditor.MouseDown e))) = Just $ handleMouseDown e
toAction (UI (AppEvent (App.MouseMove e)))               = Just $ handleMouseMove e
toAction (UI (AppEvent (App.MouseUp   _)))               = Just $ continue stopPanDrag >> continue stopZoomDrag
toAction (UI (NodeEditorEvent (NodeEditor.Wheel m w)))   = Just $ handleMouseWheel m delta where
    deltaX = fromIntegral $ -(wheelDeltaX w)
    deltaY = fromIntegral $ -(wheelDeltaY w)
    delta  = Vector2 deltaX deltaY
toAction _                                               = Nothing


-- TODO consider using state and below approach
-- init = do
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--   registerAction $ \(UI (AppEvent (App.KeyDown   e))) -> handleKey e
--


handleKey :: KeyboardEvent -> Command State ()
handleKey evt
    | Keys.withCtrl         evt Keys.leftArrow  = panLeft
    | Keys.withCtrl         evt Keys.rightArrow = panRight
    | Keys.withCtrl         evt Keys.upArrow    = panUp
    | Keys.withCtrl         evt Keys.downArrow  = panDown
    | Keys.withCtrl         evt Keys.plus       = zoomIn
    | Keys.withCtrlShift    evt Keys.plus       = zoomIn
    | Keys.withCtrl         evt Keys.minus      = zoomOut
    | Keys.withCtrlShift    evt Keys.minus      = zoomOut
    | Keys.withCtrl         evt Keys.zero       = resetZoom
    | Keys.withCtrlShift    evt Keys.zero       = resetPan
    | Keys.withCtrlAltShift evt Keys.zero       = resetCamera
    | Keys.withCtrlShift    evt Keys.zero       = centerGraph
    | Keys.withoutMods      evt Keys.h          = centerGraph
    | otherwise                                 = return ()


handleMouseDown :: MouseEvent -> Command State ()
handleMouseDown evt
    | Mouse.withoutMods evt Mouse.rightButton  = startZoomDrag $ mousePosition evt
    | Mouse.withoutMods evt Mouse.middleButton = startPanDrag  $ mousePosition evt
    | otherwise                                = return ()

handleMouseMove :: MouseEvent -> Command State ()
handleMouseMove evt
    | Mouse.withoutMods evt Mouse.rightButton  = continue $ zoomDrag $ mousePosition evt
    | Mouse.withoutMods evt Mouse.middleButton = continue $ panDrag  $ mousePosition evt
    | otherwise                                = return ()

handleMouseWheel :: MouseEvent -> Vector2 Double -> Command State ()
handleMouseWheel m delta
    | Mouse.withoutMods m Mouse.leftButton = panCamera delta
    | Mouse.withCtrl    m Mouse.leftButton = wheelZoom (mousePosition m) delta
    | otherwise                            = return ()
