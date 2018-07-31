module NodeEditor.Action.Basic.RemoveConnection where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           Control.Monad                      (filterM)
import           LunaStudio.Data.NodeLoc            (NodeLoc)
import           LunaStudio.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'))
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.State.Model      (updatePortMode)
import           NodeEditor.Action.State.NodeEditor (getConnection, getConnectionsBetweenNodes,
                                                     getConnectionsContainingNodes)
import qualified NodeEditor.Action.State.NodeEditor as NodeEditor
import           NodeEditor.React.Model.Connection  (ConnectionId, connectionId, dst, src, dstNodeLoc)
import           NodeEditor.State.Global            (State)


removeConnections :: [ConnectionId] -> Command State ()
removeConnections connIds = localRemoveConnections connIds >>= mapM_ (\connId -> do
    mayConn <- getConnection connId
    forM_ mayConn $ NodeEditor.resetSuccessors . view dstNodeLoc
    Batch.removeConnection connId)

removeConnection :: ConnectionId -> Command State ()
removeConnection = removeConnections . return

localRemoveConnections :: [ConnectionId] -> Command State [ConnectionId]
localRemoveConnections = filterM localRemoveConnection

localRemoveConnection :: ConnectionId -> Command State Bool
localRemoveConnection connId = do
    mayConn <- getConnection connId
    NodeEditor.removeConnection connId
    withJust mayConn $ \conn -> do
        NodeEditor.resetSuccessors $ conn ^. dstNodeLoc
        updatePortMode . OutPortRef' $ conn ^. src
        updatePortMode . InPortRef'  $ conn ^. dst
    return $ isJust mayConn


localRemoveConnectionsContainingNodes :: [NodeLoc] -> Command State [ConnectionId]
localRemoveConnectionsContainingNodes nls = getConnectionsContainingNodes nls >>= localRemoveConnections . map (view connectionId)

removeConnectionsBetweenNodes :: NodeLoc -> NodeLoc -> Command State ()
removeConnectionsBetweenNodes n1 n2 = getConnectionsBetweenNodes n1 n2 >>=
    removeConnections . map (view connectionId)
