module Luna.Studio.Action.Basic.UpdateConnection where

import           Empire.API.Data.Connection                (Connection, ConnectionId, connectionId)
import           Luna.Studio.Action.Basic.AddConnection    (addConnection, localAddConnection)
import           Luna.Studio.Action.Basic.RemoveConnection (localRemoveConnection, removeConnection)
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global                  (State)


updateConnection :: Connection -> ConnectionId -> Command State ()
updateConnection conn prevConnId = do
    when (conn ^. connectionId /= prevConnId) $ void $ removeConnection prevConnId
    addConnection conn

localUpdateConnection :: Connection -> ConnectionId -> Command State Bool
localUpdateConnection conn prevConnId = do
    when (conn ^. connectionId /= prevConnId) $ void $ localRemoveConnection prevConnId
    localAddConnection conn
