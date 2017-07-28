module NodeEditor.Handler.ConnectionPen
    ( handle
    ) where

import           Common.Action.Command           (Command)
import           Common.Prelude
import           Data.Timestamp                  (Timestamp)
import           NodeEditor.Action.ConnectionPen (connectMove, disconnectMove, startConnecting, startDisconnecting, stopConnecting,
                                                  stopDisconnecting)
import           NodeEditor.Event.Event          (Event, Event (UI))
import qualified NodeEditor.Event.Mouse          as Mouse
import           NodeEditor.Event.UI             (UIEvent (AppEvent))
import qualified NodeEditor.React.Event.App      as App
import           NodeEditor.State.Action         (Action (continue))
import           NodeEditor.State.Global         (State)
import           React.Flux                      (MouseEvent)


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
