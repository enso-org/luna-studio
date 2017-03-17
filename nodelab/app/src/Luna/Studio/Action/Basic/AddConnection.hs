module Luna.Studio.Action.Basic.AddConnection where

import           Control.Monad                           (filterM)
import           Empire.API.Data.Connection              (Connection (Connection), ConnectionId, connectionId, dst, dstNodeId, dstPortId,
                                                          src)
import           Empire.API.Data.Node                    (NodeId)
import           Empire.API.Data.Port                    (InPort (Self))
import           Empire.API.Data.PortRef                 (AnyPortRef (InPortRef'), InPortRef, OutPortRef)
import           Luna.Studio.Action.Basic.DrawConnection (drawConnection)
import           Luna.Studio.Action.Basic.UpdateNode     (updatePortSelfVisibility)
import qualified Luna.Studio.Action.Batch                as Batch
import           Luna.Studio.Action.Command              (Command)
import qualified Luna.Studio.Action.State.Graph          as Graph
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global                (State)


addConnection :: Connection -> Command State ()
addConnection conn = connect (Left $ conn ^. src) (Left $ conn ^. dst)

connect :: Either OutPortRef NodeId -> Either InPortRef NodeId -> Command State ()
connect src'@(Left srcPortRef) (Left dstPortRef) =
    whenM (localConnect srcPortRef dstPortRef) $ Batch.addConnection src' (Left $ InPortRef' dstPortRef)
connect src' (Left dstPortRef) = Batch.addConnection src' (Left $ InPortRef' dstPortRef)
connect src' (Right nid)       = Batch.addConnection src' (Right nid)


localConnect :: OutPortRef -> InPortRef -> Command State Bool
localConnect src' dst' = localAddConnection $ Connection src' dst'

localAddConnections :: [Connection] -> Command State [ConnectionId]
localAddConnections conns = map (view connectionId) <$> filterM localAddConnection conns

localAddConnection :: Connection -> Command State Bool
localAddConnection connection = do
    inNodeEditor <- drawConnection connection
    if not inNodeEditor
        then return False
        else do
            Graph.addConnection connection
            when (Self == connection ^. dstPortId) . void . updatePortSelfVisibility $ connection ^. dstNodeId
            return True
