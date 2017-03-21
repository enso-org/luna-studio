module Luna.Studio.Handler.ConnectionPen
    ( handle
    ) where

import           Data.Timestamp                   (Timestamp)
import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Action.ConnectionPen (connectMove, disconnectMove, startConnecting, startDisconnecting, stopConnecting,
                                                   stopDisconnecting)
import           Luna.Studio.Event.Event          (Event, Event (UI))
import qualified Luna.Studio.Event.Mouse          as Mouse
import           Luna.Studio.Event.UI             (UIEvent (AppEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App      as App
import           Luna.Studio.State.Action         (Action (continue))
import           Luna.Studio.State.Global         (State)
import           React.Flux                       (MouseEvent)


handle :: Event -> Maybe (Command State ())
handle (UI (AppEvent (App.MouseDown evt timestamp))) = Just $ handleMouseDown evt timestamp
handle (UI (AppEvent (App.MouseMove evt timestamp))) = Just $ continue (connectMove evt timestamp) >> continue (disconnectMove evt timestamp)
handle (UI (AppEvent (App.MouseUp   _)))             = Just $ continue stopConnecting >> continue stopDisconnecting
handle _                                             = Nothing

handleMouseDown :: MouseEvent -> Timestamp -> Command State ()
handleMouseDown evt timestamp
    | Mouse.withCtrl      evt Mouse.leftButton  = startConnecting    evt timestamp
    | Mouse.withCtrlShift evt Mouse.leftButton  = startDisconnecting evt timestamp
    | Mouse.withCtrl      evt Mouse.rightButton = startDisconnecting evt timestamp
    | otherwise                                 = return ()
