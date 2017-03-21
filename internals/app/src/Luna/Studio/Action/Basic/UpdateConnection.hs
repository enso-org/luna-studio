module Luna.Studio.Action.Basic.UpdateConnection where

import           Empire.API.Data.Connection                (Connection, ConnectionId)
import           Luna.Studio.Action.Basic.AddConnection    (addConnection, localAddConnection)
import           Luna.Studio.Action.Basic.RemoveConnection (localRemoveConnection, removeConnection)
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global                  (State)


updateConnection :: Connection -> ConnectionId -> Command State ()
updateConnection conn prevConnId = undefined --do
    -- addConnection conn
    -- when (conn ^. connectionId /= prevConnId) $ void $ removeConnection prevConnId

localUpdateConnection :: Connection -> ConnectionId -> Command State Bool
localUpdateConnection conn prevConnId = undefined --do
    -- when (conn ^. connectionId /= prevConnId) $ void $ localRemoveConnection prevConnId
    -- localAddConnection conn
