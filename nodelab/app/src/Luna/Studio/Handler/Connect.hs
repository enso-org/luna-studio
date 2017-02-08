module Luna.Studio.Handler.Connect
    ( handle
    ) where

import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         (dragConnectToPort, dragModifyConnection, handleClickConnect, handleDragConnectMouseUp,
                                                     handleMove, startDragConnect, stopClickConnect, whileConnecting)
import           Luna.Studio.Event.Event            (Event (UI))
import           Luna.Studio.Event.UI               (UIEvent (AppEvent, ConnectionEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.Connection as Connection
import           Luna.Studio.State.Action           (Action (continue))
import           Luna.Studio.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (ConnectionEvent (Connection.StartConnection evt portRef)))     = Just $ startDragConnect   evt portRef
handle (UI (ConnectionEvent (Connection.Click evt portRef)))               = Just $ handleClickConnect evt portRef
handle (UI (AppEvent (App.MouseMove evt _)))                               = Just $ whileConnecting $ handleMove evt
handle (UI (AppEvent (App.MouseUp   evt  )))                               = Just $ continue $ handleDragConnectMouseUp evt
handle (UI (AppEvent (App.Click     _    )))                               = Just $ continue   stopClickConnect
handle (UI (ConnectionEvent (Connection.EndConnection _ portRef)))         = Just $ continue $ dragConnectToPort portRef
handle (UI (ConnectionEvent (Connection.ModifyConnection evt connId end))) = Just $ dragModifyConnection evt connId end
handle _                                                                   = Nothing
