module Node.Editor.Handler.ConnectionPen
    ( handle
    ) where

import           Data.Timestamp                   (Timestamp)
import           Node.Editor.Action.Command       (Command)
import           Node.Editor.Action.ConnectionPen (connectMove, disconnectMove, startConnecting, startDisconnecting, stopConnecting,
                                                   stopDisconnecting)
import           Node.Editor.Event.Event          (Event, Event (UI))
import qualified Node.Editor.Event.Mouse          as Mouse
import           Node.Editor.Event.UI             (UIEvent (AppEvent))
import           Luna.Prelude
import qualified Node.Editor.React.Event.App      as App
import           Node.Editor.State.Action         (Action (continue))
import           Node.Editor.State.Global         (State)
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
