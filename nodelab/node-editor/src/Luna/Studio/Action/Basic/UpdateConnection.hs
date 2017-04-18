module Luna.Studio.Action.Basic.UpdateConnection where

import           Luna.Studio.Action.Basic.AddConnection    (connect, localAddConnection)
import           Luna.Studio.Action.Basic.RemoveConnection (localRemoveConnection, removeConnection)
import           Luna.Studio.Action.Command                (Command)
import           Luna.Prelude
import           Luna.Studio.React.Model.Connection        (Connection, ConnectionId, connectionId, dst, src)
import           Luna.Studio.State.Global                  (State)


updateConnection :: Connection -> ConnectionId -> Command State ()
updateConnection conn prevConnId = do
    when (conn ^. connectionId /= prevConnId) $ void $ removeConnection prevConnId
    connect (Left $ conn ^. src) (Left $ conn ^. dst)

localUpdateConnection :: Connection -> ConnectionId -> Command State Bool
localUpdateConnection conn prevConnId = do
    when (conn ^. connectionId /= prevConnId) $ void $ localRemoveConnection prevConnId
    localAddConnection (conn ^. src) (conn ^. dst)
