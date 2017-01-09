module Luna.Studio.Action.Connect
    ( toAction
    ) where

import           Luna.Studio.Prelude

import           Event.Event                        (Event (UI))
import           Event.UI                           (UIEvent (AppEvent, ConnectionEvent))
import qualified Luna.Studio.React.Event.App        as App
import qualified Luna.Studio.React.Event.Connection as Connection

import           Luna.Studio.Commands.Command       (Command)
import           Luna.Studio.Commands.Connect       (connectToPort, handleMove, modifyConnection, startDragFromPort, stopDrag,
                                                     whileConnecting)
import           Luna.Studio.State.Global           (State)


toAction :: Event -> Maybe (Command State ())
toAction (UI (ConnectionEvent (Connection.StartConnection evt portRef)))     = Just $ startDragFromPort evt portRef Nothing
toAction (UI (AppEvent  (App.MouseMove evt)))                                = Just $ whileConnecting $ handleMove evt
toAction (UI (AppEvent (App.MouseUp _)))                                     = Just $ whileConnecting $ stopDrag
toAction (UI (ConnectionEvent (Connection.EndConnection _ portRef)))         = Just $ whileConnecting $ connectToPort portRef
toAction (UI (ConnectionEvent (Connection.ModifyConnection evt connId end))) = Just $ modifyConnection evt connId end
toAction _                                                                   = Nothing
