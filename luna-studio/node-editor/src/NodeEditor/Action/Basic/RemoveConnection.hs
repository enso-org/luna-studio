module NodeEditor.Action.Basic.RemoveConnection where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           Control.Monad                      (filterM)
import           LunaStudio.Data.NodeLoc            (NodeLoc)
import           LunaStudio.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'))
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.State.Model      (updatePortMode)
import           NodeEditor.Action.State.NodeEditor (getConnection, getConnectionsBetweenNodes, getConnectionsContainingNode,
                                                     getConnectionsContainingNodes)
import qualified NodeEditor.Action.State.NodeEditor as NodeEditor
import           NodeEditor.React.Model.Connection  (ConnectionId, connectionId, dst, src)
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
    mayConn <- getConnection connId
    NodeEditor.removeConnection connId
    withJust mayConn $ \conn -> do
        updatePortMode . OutPortRef' $ conn ^. src
        updatePortMode . InPortRef'  $ conn ^. dst
    return $ isJust mayConn

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
