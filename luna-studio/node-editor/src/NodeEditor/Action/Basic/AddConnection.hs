module NodeEditor.Action.Basic.AddConnection where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           Control.Monad                      (filterM)
import           LunaStudio.Data.Connection         (Connection (Connection))
import qualified LunaStudio.Data.Connection         as Connection
import           LunaStudio.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.State.Model      (createConnectionModel, updatePortMode)
import qualified NodeEditor.Action.State.NodeEditor as NodeEditor
import           NodeEditor.React.Model.Connection  (ConnectionId)
import           NodeEditor.React.Model.Node        (NodeLoc, nodeLoc)
import           NodeEditor.State.Global            (State)


connect :: Either OutPortRef NodeLoc -> Either InPortRef NodeLoc
    -> Command State ()
connect src'@(Left srcPortRef) (Left dstPortRef)
    = whenM (localAddConnection $ Connection srcPortRef dstPortRef) $ do
        NodeEditor.resetSuccessors $ dstPortRef ^. nodeLoc
        Batch.addConnection src' (Left $ InPortRef' dstPortRef)
connect src' (Left dstPortRef) = do
    NodeEditor.resetSuccessors $ dstPortRef ^. nodeLoc
    Batch.addConnection src' (Left $ InPortRef' dstPortRef)
connect src' (Right nl) = do
    NodeEditor.resetSuccessors nl
    Batch.addConnection src' (Right nl)

localAddConnections :: [Connection] -> Command State [ConnectionId]
localAddConnections
    = fmap2 (view Connection.connectionId) . filterM localAddConnection

localAddConnection :: Connection -> Command State Bool
localAddConnection c = do
    let src = c ^. Connection.src
        dst = c ^. Connection.dst
    mayConn <- createConnectionModel src dst
    withJust mayConn $ \conn -> do
        NodeEditor.addConnection conn
        updatePortMode $ OutPortRef' src
        updatePortMode $ InPortRef'  dst
    return $ isJust mayConn
