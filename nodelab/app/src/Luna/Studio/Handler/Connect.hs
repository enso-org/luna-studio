module Luna.Studio.Handler.Connect
    ( handle
    ) where

import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         (cancelSnapToPort, handleClick, handleConnectionMouseDown, handleMouseUp, handleMove,
                                                     handlePortMouseDown, handlePortMouseUp, snapToPort, stopConnecting)
import           Luna.Studio.Event.Event            (Event (UI))
import           Luna.Studio.Event.UI               (UIEvent (AppEvent, ConnectionEvent, PortEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.Connection as Connection
import qualified Luna.Studio.React.Event.Port       as Port
import           Luna.Studio.State.Action           (Action (continue))
import           Luna.Studio.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (PortEvent       (Port.MouseDown       evt portRef   ))) = Just $ handlePortMouseDown       evt portRef
handle (UI (ConnectionEvent (Connection.MouseDown evt connId end))) = Just $ handleConnectionMouseDown evt connId end
handle (UI (PortEvent       (Port.Click           evt portRef   ))) = Just $ handleClick               evt portRef
handle (UI (AppEvent        (App.MouseMove        evt _         ))) = Just $ continue $ handleMove evt
handle (UI (AppEvent        (App.MouseUp          evt           ))) = Just $ continue $ handleMouseUp evt
handle (UI (AppEvent        (App.Click                          ))) = Just $ continue   stopConnecting
handle (UI (PortEvent       (Port.MouseUp             portRef   ))) = Just $ continue $ handlePortMouseUp portRef
handle (UI (PortEvent       (Port.MouseEnter          portRef   ))) = Just $ continue $ snapToPort portRef
handle (UI (PortEvent       (Port.MouseLeave          portRef   ))) = Just $ continue $ cancelSnapToPort portRef
handle _                                                            = Nothing
