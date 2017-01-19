module Luna.Studio.Handler.ConnectionPen
    ( toAction
    ) where

import           Event.Event                      (Event)
import           Event.Event                      (Event (UI))
import           Event.UI                         (UIEvent (AppEvent))
import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Action.ConnectionPen (connectMove, disconnectMove, startConnecting, startDisconnecting, stopConnecting,
                                                   stopDisconnecting)
import qualified Luna.Studio.Event.Mouse          as Mouse
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App      as App
import           Luna.Studio.State.Action         (Action (continue))
import           Luna.Studio.State.Global         (State)
import           React.Flux                       (MouseEvent)


toAction :: Event -> Maybe (Command State ())
toAction (UI (AppEvent (App.MouseDown evt))) = Just $ handleMouseDown evt
toAction (UI (AppEvent (App.MouseMove evt))) = Just $ handleMouseMove evt
toAction (UI (AppEvent (App.MouseUp   _  ))) = Just $ continue stopConnecting >> continue stopDisconnecting
toAction _                                   = Nothing

handleMouseDown :: MouseEvent -> Command State ()
handleMouseDown evt
    | Mouse.withCtrl      evt Mouse.leftButton  = startConnecting    evt
    | Mouse.withCtrlShift evt Mouse.leftButton  = startDisconnecting evt
    | Mouse.withCtrl      evt Mouse.rightButton = startDisconnecting evt
    | otherwise                                 = return ()

handleMouseMove :: MouseEvent -> Command State ()
handleMouseMove evt
    | Mouse.withCtrl      evt Mouse.leftButton  = continue $ connectMove evt
    | Mouse.withCtrlShift evt Mouse.leftButton  = continue $ disconnectMove evt
    | Mouse.withCtrl      evt Mouse.leftButton  = continue $ disconnectMove evt
    | otherwise                                 = return ()
