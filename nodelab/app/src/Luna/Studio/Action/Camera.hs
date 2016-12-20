{-# LANGUAGE NamedFieldPuns #-}
module Luna.Studio.Action.Camera
    ( toAction
    ) where

import           Event.Event                  (Event (UI))
import qualified Event.Keys                   as Keys
import           Event.UI                     (UIEvent (AppEvent))
import           Luna.Studio.Commands.Camera  (autoZoom, panCamera, panDown, panDrag, panLeft, panRight, panUp, resetZoom, syncCamera,
                                               wheelZoom, zoomDrag, zoomIn, zoomOut)
import           Luna.Studio.Commands.Command (Command)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App  as App
import qualified Luna.Studio.State.Camera     as Camera
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.State.Global     as Global
import           React.Flux                   (KeyboardEvent)


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
-- toAction' (Keyboard _ (Keyboard.Event Keyboard.Press char _)) = case char of
--     '='   -> Just $ zoomIn
--     '+'   -> Just $ zoomIn
--     '-'   -> Just $ zoomOut
--     '0'   -> Just $ resetZoom
--     _     -> Nothing
--
-- toAction' (Keyboard _ (Keyboard.Event Keyboard.Down char (KeyMods False True False False))) = case char of
--     '\37' -> Just panLeft
--     '\39' -> Just panRight
--     '\38' -> Just panUp
--     '\40' -> Just panDown
--     _     -> Nothing
-- toAction' _ = Nothing

toAction :: Event -> Maybe (Command State ())
toAction (UI (AppEvent (App.KeyDown e))) = Just $ handleKey e
toAction _ = Nothing

handleKey :: KeyboardEvent -> Command State ()
handleKey evt
    | Keys.withCtrl evt Keys.leftArrow  = panLeft
    | Keys.withCtrl evt Keys.rightArrow = panRight
    | Keys.withCtrl evt Keys.upArrow    = panUp
    | Keys.withCtrl evt Keys.downArrow  = panDown
    | otherwise                         = return ()
