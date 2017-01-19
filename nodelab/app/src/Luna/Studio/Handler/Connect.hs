module Luna.Studio.Handler.Connect
    ( toAction
    ) where

import           Event.Event                        (Event (UI))
import           Event.UI                           (UIEvent (AppEvent, ConnectionEvent))
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         (connectToPort, handleMove, modifyConnection, startOrModifyConnection, stopConnecting,
                                                     whileConnecting)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.Connection as Connection
import           Luna.Studio.State.Global           (State)


toAction :: Event -> Maybe (Command State ())
toAction (UI (ConnectionEvent (Connection.StartConnection evt portRef)))     = Just $ startOrModifyConnection evt portRef
toAction (UI (AppEvent  (App.MouseMove evt)))                                = Just $ whileConnecting $ handleMove evt
toAction (UI (AppEvent (App.MouseUp _)))                                     = Just $ whileConnecting $ stopConnecting
toAction (UI (ConnectionEvent (Connection.EndConnection _ portRef)))         = Just $ whileConnecting $ connectToPort portRef
toAction (UI (ConnectionEvent (Connection.ModifyConnection evt connId end))) = Just $ modifyConnection evt connId end
toAction _                                                                   = Nothing
