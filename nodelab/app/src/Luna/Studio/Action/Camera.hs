{-# LANGUAGE NamedFieldPuns #-}
module Luna.Studio.Action.Camera
    ( toAction
    ) where

import           Event.Event                        (Event (UI))
import           Event.UI                           (UIEvent (AppEvent, NodeEditorEvent))
import           Luna.Studio.Commands.Camera
import           Luna.Studio.Commands.Command       (Command)
import qualified Luna.Studio.Event.Keys             as Keys
import           Luna.Studio.Event.Mouse            (mousePosition)
import qualified Luna.Studio.Event.Mouse            as Mouse
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.NodeEditor as NodeEditor
import           Luna.Studio.State.Global           (State)
import           React.Flux                         (KeyboardEvent)
-- toAction :: Event -> Maybe (Command Global.State ())
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Press 'h' _)) = Just $ autoZoom
-- toAction evt = (zoom Global.camera) <$> (>> syncCamera) <$> toAction' evt
--
-- toAction' :: Event -> Maybe (Command Camera.State ())
-- toAction' (Mouse _ (Mouse.Event evt pos RightButton  KeyMods {_ctrl = False} _)) = Just $ zoomDrag evt pos
-- toAction' (Mouse _ (Mouse.Event evt pos MiddleButton _ _)) = Just $ panDrag  evt pos
--
-- toAction' (Mouse _ (Mouse.Event (Mouse.Wheel delta) _   _ KeyMods {_ctrl = False} _)) = Just $ panCamera delta
-- toAction' (Mouse _ (Mouse.Event (Mouse.Wheel delta) pos _ KeyMods {_ctrl = True} _))  = Just $ wheelZoom pos delta
--
-- toAction' _ = Nothing

toAction :: Event -> Maybe (Command State ())
toAction (UI (AppEvent (App.KeyDown   e))) = Just $ handleKey e
toAction (UI (NodeEditorEvent (NodeEditor.MouseDown e))) = Just $ when shouldProceed $ startZoomDrag pos where
    shouldProceed = Mouse.withoutMods e Mouse.rightButton
    pos           = mousePosition e
toAction (UI (AppEvent (App.MouseMove e))) = Just $ zoomDrag pos where
    pos = mousePosition e
toAction (UI (AppEvent (App.MouseUp   _))) = Just stopZoomDrag
toAction _                                 = Nothing


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
    | Keys.withCtrlShift    evt Keys.zero       = autoZoom
    | Keys.withoutMods      evt Keys.h          = autoZoom
    | otherwise                                 = return ()
