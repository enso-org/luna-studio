module NodeEditor.Action.Basic.RemoveConnection where

import           Control.Monad                       (filterM)
import           LunaStudio.Data.NodeLoc             (NodeLoc)
import           LunaStudio.Data.PortRef             (dstNodeLoc)
import           NodeEditor.Action.Basic.UpdateNode (updatePortSelfVisibility)
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.NodeEditor (getConnectionsBetweenNodes, getConnectionsContainingNode,
                                                      getConnectionsContainingNodes, inGraph)
import qualified NodeEditor.Action.State.NodeEditor as NodeEditor
import           Common.Prelude
import           NodeEditor.React.Model.Connection  (ConnectionId, connectionId)
import           NodeEditor.State.Global            (State)


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
