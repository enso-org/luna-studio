module Node.Editor.Action.Basic.UpdateConnection where

import           Node.Editor.Action.Basic.AddConnection    (connect, localAddConnection)
import           Node.Editor.Action.Basic.RemoveConnection (localRemoveConnection, removeConnection)
import           Node.Editor.Action.Command                (Command)
import           Luna.Prelude
import           Node.Editor.React.Model.Connection        (Connection, ConnectionId, connectionId, dst, src)
import           Node.Editor.State.Global                  (State)


updateConnection :: Connection -> ConnectionId -> Command State ()
updateConnection conn prevConnId = do
    when (conn ^. connectionId /= prevConnId) $ void $ removeConnection prevConnId
    connect (Left $ conn ^. src) (Left $ conn ^. dst)

localUpdateConnection :: Connection -> ConnectionId -> Command State Bool
localUpdateConnection conn prevConnId = do
    when (conn ^. connectionId /= prevConnId) $ void $ localRemoveConnection prevConnId
    localAddConnection (conn ^. src) (conn ^. dst)
