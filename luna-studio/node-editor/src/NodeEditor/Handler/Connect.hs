module NodeEditor.Handler.Connect
    ( handle
    ) where

import           Common.Action.Command             (Command)
import           Common.Prelude
import           NodeEditor.Action.Connect         (cancelSnapToPort, handleConnectionMouseDown, handleMouseUp, handleMove,
                                                    handlePortMouseUp, snapToPort)
import           NodeEditor.Event.Event            (Event (UI))
import           NodeEditor.Event.UI               (UIEvent (AppEvent, ConnectionEvent, PortEvent))
import qualified NodeEditor.React.Event.App        as App
import qualified NodeEditor.React.Event.Connection as Connection
import qualified NodeEditor.React.Event.Port       as Port
import           NodeEditor.State.Action           (Action (continue, end), Connect)
import           NodeEditor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (ConnectionEvent (Connection.MouseDown evt connId end'))) = Just $ handleConnectionMouseDown evt connId end'
handle (UI (AppEvent        (App.MouseMove        evt _          ))) = Just $ continue $ handleMove evt
handle (UI (AppEvent        (App.MouseUp          evt            ))) = Just $ continue $ handleMouseUp evt
handle (UI (AppEvent        (App.Click                           ))) = Just $ continue $ (end :: Connect -> Command State ())
handle (UI (PortEvent       (Port.MouseUp             portRef    ))) = Just $ continue $ handlePortMouseUp portRef
handle (UI (PortEvent       (Port.MouseEnter          portRef    ))) = Just $ continue $ snapToPort portRef
handle (UI (PortEvent       (Port.MouseLeave          portRef    ))) = Just $ continue $ cancelSnapToPort portRef
handle _                                                             = Nothing
