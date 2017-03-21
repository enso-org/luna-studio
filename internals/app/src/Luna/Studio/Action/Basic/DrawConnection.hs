module Luna.Studio.Action.Basic.DrawConnection where

import           Control.Monad                       (filterM)
import           Empire.API.Data.Connection          (Connection, ConnectionId)
import           Empire.API.Data.Node                (NodeId, nodeId)
import           JS.Scene                            (inputSidebar, outputSidebar)
import           Luna.Studio.Action.Command          (Command)
-- import qualified Luna.Studio.Action.State.Graph      as Graph
import           Luna.Studio.Action.State.Model      (createConnectionModel)
import           Luna.Studio.Action.State.NodeEditor (modifyNodeEditor)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.NodeEditor  (connections)
import           Luna.Studio.State.Global            (State, scene)


drawConnection :: Connection -> Command State Bool
drawConnection conn = do
    prevInputSidebar  <- (fmap . fmap) (view inputSidebar)  $ use scene
    prevOutputSidebar <- (fmap . fmap) (view outputSidebar) $ use scene
    mayConnModel      <- createConnectionModel conn
    withJust mayConnModel NodeEditor.addConnection
    newInputSidebar  <- (fmap . fmap) (view inputSidebar)  $ use scene
    newOutputSidebar <- (fmap . fmap) (view outputSidebar) $ use scene
    when (newInputSidebar /= prevInputSidebar || newOutputSidebar /= prevOutputSidebar) $
        void redrawConnectionsForEdgeNodes
    return $ isJust mayConnModel

redrawConnection :: ConnectionId -> Command State Bool
redrawConnection connId = undefined --Graph.getConnection connId >>=
    -- maybe (return False) drawConnection

redrawConnections :: Command State ()
redrawConnections = undefined --do
    -- modifyNodeEditor $ connections .= def
    -- Graph.getConnections >>= mapM_ drawConnection

redrawConnectionsForNode :: NodeId -> Command State [ConnectionId]
redrawConnectionsForNode nid = undefined --Graph.getConnectionsContainingNode nid >>=
    -- (fmap . map) (view connectionId) . filterM drawConnection

--TODO[LJK]: Should we remove all previous conns for nodes?
redrawConnectionsForNodes :: [NodeId] -> Command State [ConnectionId]
redrawConnectionsForNodes nodeIds = undefined --mapM Graph.getConnectionsContainingNode nodeIds >>=
    -- (fmap . map) (view connectionId) . filterM drawConnection . join


redrawConnectionsForEdgeNodes :: Command State [ConnectionId]
redrawConnectionsForEdgeNodes = undefined --Graph.getEdgeNodes >>=
    -- redrawConnectionsForNodes . (map (view nodeId))
