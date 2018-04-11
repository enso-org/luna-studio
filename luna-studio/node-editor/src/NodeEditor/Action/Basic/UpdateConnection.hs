module NodeEditor.Action.Basic.UpdateConnection where

import           Common.Action.Command                    (Command)
import           Common.Prelude
import           NodeEditor.Action.Basic.AddConnection    (connect, localAddConnection)
import           NodeEditor.Action.Basic.RemoveConnection (localRemoveConnection, removeConnection)
import           NodeEditor.React.Model.Connection        (Connection, ConnectionId, connectionId, dst, src)
import           NodeEditor.State.Global                  (State)


updateConnection :: Connection -> ConnectionId -> Command State ()
updateConnection conn prevConnId = do
    when (conn ^. connectionId /= prevConnId)
        . void $ removeConnection prevConnId
    connect (Left $ conn ^. src) (Left $ conn ^. dst)

localUpdateConnection :: Connection -> ConnectionId -> Command State Bool
localUpdateConnection conn prevConnId = do
    when (conn ^. connectionId /= prevConnId)
        . void $ localRemoveConnection prevConnId
    localAddConnection $ convert conn
