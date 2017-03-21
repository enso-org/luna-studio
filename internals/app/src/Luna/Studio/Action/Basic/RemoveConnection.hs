module Luna.Studio.Action.Basic.RemoveConnection where

import           Control.Monad                       (filterM)
import           Empire.API.Data.Connection          (ConnectionId)
import           Empire.API.Data.Node                (NodeId)
import           Empire.API.Data.PortRef             (dstNodeId)
import           Luna.Studio.Action.Basic.UpdateNode (updatePortSelfVisibility)
import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.Graph      (getConnectionsBetweenNodes, getConnectionsContainingNode,
                                                      getConnectionsContainingNodes)
import qualified Luna.Studio.Action.State.Graph      as Graph
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global            (State)

removeConnections :: [ConnectionId] -> Command State ()
removeConnections connIds = undefined --localRemoveConnections connIds >>= \connToRemoveIds ->
    -- unless (null connToRemoveIds) $ mapM_ Batch.removeConnection connToRemoveIds

removeConnection :: ConnectionId -> Command State ()
removeConnection connId =
    undefined --whenM (localRemoveConnection connId) $ Batch.removeConnection connId

localRemoveConnections :: [ConnectionId] -> Command State [ConnectionId]
localRemoveConnections = undefined --filterM localRemoveConnection

localRemoveConnection :: ConnectionId -> Command State Bool
localRemoveConnection connId = undefined --do
    -- mayConn <- Graph.getConnection connId
    -- Graph.removeConnection connId
    -- NodeEditor.removeConnection connId
    -- void . updatePortSelfVisibility $ connId ^. dstNodeId
    -- return $ isJust mayConn


localRemoveConnectionsContainingNode :: NodeId -> Command State [ConnectionId]
localRemoveConnectionsContainingNode nid = undefined --getConnectionsContainingNode nid >>= localRemoveConnections . map (view connectionId)

localRemoveConnectionsContainingNodes :: [NodeId] -> Command State [ConnectionId]
localRemoveConnectionsContainingNodes nids = undefined --getConnectionsContainingNodes nids >>= localRemoveConnections . map (view connectionId)

removeConnectionsBetweenNodes :: NodeId -> NodeId -> Command State ()
removeConnectionsBetweenNodes n1 n2 =undefined -- getConnectionsBetweenNodes n1 n2 >>=
    -- removeConnections . map (view connectionId)

localRemoveConnectionsBetweenNodes :: NodeId -> NodeId -> Command State [ConnectionId]
localRemoveConnectionsBetweenNodes n1 n2 = undefined --getConnectionsBetweenNodes n1 n2 >>=
    -- localRemoveConnections . map (view connectionId)
