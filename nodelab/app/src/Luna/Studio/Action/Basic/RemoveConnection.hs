module Luna.Studio.Action.Basic.RemoveConnection where

import           Control.Monad                       (filterM)
import           Empire.API.Data.NodeLoc             (NodeLoc)
import           Empire.API.Data.PortRef             (dstNodeLoc)
import           Luna.Studio.Action.Basic.UpdateNode (updatePortSelfVisibility)
import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.NodeEditor (getConnectionsBetweenNodes, getConnectionsContainingNode,
                                                      getConnectionsContainingNodes, inGraph)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection  (ConnectionId, connectionId)
import           Luna.Studio.State.Global            (State)


removeConnections :: [ConnectionId] -> Command State ()
removeConnections connIds = localRemoveConnections connIds >>= \connToRemoveIds ->
    unless (null connToRemoveIds) $ mapM_ Batch.removeConnection connToRemoveIds

removeConnection :: ConnectionId -> Command State ()
removeConnection connId =
    whenM (localRemoveConnection connId) $ Batch.removeConnection connId

localRemoveConnections :: [ConnectionId] -> Command State [ConnectionId]
localRemoveConnections = filterM localRemoveConnection

localRemoveConnection :: ConnectionId -> Command State Bool
localRemoveConnection connId = do
    result <- inGraph connId
    NodeEditor.removeConnection connId
    void . updatePortSelfVisibility $ connId ^. dstNodeLoc
    return result


localRemoveConnectionsContainingNode :: NodeLoc -> Command State [ConnectionId]
localRemoveConnectionsContainingNode nl = getConnectionsContainingNode nl >>= localRemoveConnections . map (view connectionId)

localRemoveConnectionsContainingNodes :: [NodeLoc] -> Command State [ConnectionId]
localRemoveConnectionsContainingNodes nls = getConnectionsContainingNodes nls >>= localRemoveConnections . map (view connectionId)

removeConnectionsBetweenNodes :: NodeLoc -> NodeLoc -> Command State ()
removeConnectionsBetweenNodes n1 n2 = getConnectionsBetweenNodes n1 n2 >>=
    removeConnections . map (view connectionId)

localRemoveConnectionsBetweenNodes :: NodeLoc -> NodeLoc -> Command State [ConnectionId]
localRemoveConnectionsBetweenNodes n1 n2 = getConnectionsBetweenNodes n1 n2 >>=
    localRemoveConnections . map (view connectionId)
