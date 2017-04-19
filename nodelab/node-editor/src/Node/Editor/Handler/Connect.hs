module Node.Editor.Handler.Connect
    ( handle
    ) where

import           Node.Editor.Action.Command         (Command)
import           Node.Editor.Action.Connect         (cancelSnapToPort, handleConnectionMouseDown, handleMouseUp, handleMove,
                                                     handlePortMouseUp, snapToPort)
import           Node.Editor.Event.Event            (Event (UI))
import           Node.Editor.Event.UI               (UIEvent (AppEvent, ConnectionEvent, PortEvent))
import           Luna.Prelude
import qualified Node.Editor.React.Event.App        as App
import qualified Node.Editor.React.Event.Connection as Connection
import qualified Node.Editor.React.Event.Port       as Port
import           Node.Editor.State.Action           (Action (continue, end), Connect)
import           Node.Editor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (ConnectionEvent (Connection.MouseDown evt connId end'))) = Just $ handleConnectionMouseDown evt connId end'
handle (UI (AppEvent        (App.MouseMove        evt _          ))) = Just $ continue $ handleMove evt
handle (UI (AppEvent        (App.MouseUp          evt            ))) = Just $ continue $ handleMouseUp evt
handle (UI (AppEvent        (App.Click                           ))) = Just $ continue $ (end :: Connect -> Command State ())
handle (UI (PortEvent       (Port.MouseUp             portRef    ))) = Just $ continue $ handlePortMouseUp portRef
handle (UI (PortEvent       (Port.MouseEnter          portRef    ))) = Just $ continue $ snapToPort portRef
handle (UI (PortEvent       (Port.MouseLeave          portRef    ))) = Just $ continue $ cancelSnapToPort portRef
handle _                                                             = Nothing
