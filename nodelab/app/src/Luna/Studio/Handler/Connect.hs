module Luna.Studio.Handler.Connect
    ( toAction
    ) where

import           Event.Event                        (Event (UI))
import           Event.UI                           (UIEvent (AppEvent, ConnectionEvent))
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         (dragConnectToPort, dragModifyConnection, handleClickConnect, handleDragConnectMouseUp,
                                                     handleMove, startDragConnect, stopClickConnect, whileConnecting)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.Connection as Connection
import           Luna.Studio.State.Action           (Action (continue))
import           Luna.Studio.State.Global           (State)


toAction :: Event -> Maybe (Command State ())
toAction (UI (ConnectionEvent (Connection.StartConnection evt portRef)))     = Just $ startDragConnect  evt portRef
toAction (UI (ConnectionEvent (Connection.Click evt portRef)))               = Just $ handleClickConnect evt portRef
toAction (UI (AppEvent  (App.MouseMove evt)))                                = Just $ whileConnecting $ handleMove evt
toAction (UI (AppEvent (App.MouseUp evt)))                                   = Just $ continue $ handleDragConnectMouseUp evt
toAction (UI (AppEvent (App.Click _)))                                       = Just $ continue $ stopClickConnect
toAction (UI (ConnectionEvent (Connection.EndConnection _ portRef)))         = Just $ continue $ dragConnectToPort portRef
toAction (UI (ConnectionEvent (Connection.ModifyConnection evt connId end))) = Just $ dragModifyConnection evt connId end
toAction _                                                                   = Nothing
