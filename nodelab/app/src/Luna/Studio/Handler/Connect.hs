module Luna.Studio.Handler.Connect
    ( handle
    ) where

import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         (cancelSnapToPort, handleConnectionMouseDown, handleMouseUp, handleMove,
                                                     handlePortMouseUp, snapToPort)
import           Luna.Studio.Event.Event            (Event (UI))
import           Luna.Studio.Event.UI               (UIEvent (AppEvent, ConnectionEvent, PortEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.Connection as Connection
import qualified Luna.Studio.React.Event.Port       as Port
import           Luna.Studio.State.Action           (Action (continue, end), Connect)
import           Luna.Studio.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (ConnectionEvent (Connection.MouseDown evt connId end'))) = Just $ handleConnectionMouseDown evt connId end'
handle (UI (AppEvent        (App.MouseMove        evt _          ))) = Just $ continue $ handleMove evt
handle (UI (AppEvent        (App.MouseUp          evt            ))) = Just $ continue $ handleMouseUp evt
handle (UI (AppEvent        (App.Click                           ))) = Just $ continue $ (end :: Connect -> Command State ())
handle (UI (PortEvent       (Port.MouseUp             portRef    ))) = Just $ continue $ handlePortMouseUp portRef
handle (UI (PortEvent       (Port.MouseEnter          portRef    ))) = Just $ continue $ snapToPort portRef
handle (UI (PortEvent       (Port.MouseLeave          portRef    ))) = Just $ continue $ cancelSnapToPort portRef
handle _                                                             = Nothing
