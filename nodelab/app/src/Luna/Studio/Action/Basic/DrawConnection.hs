module Luna.Studio.Action.Basic.DrawConnection where

import           Control.Monad                       (filterM)
import qualified Empire.API.Data.Connection          as API
import           JS.Scene                            (inputSidebar, outputSidebar)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.Model      (createConnectionModel)
import           Luna.Studio.Action.State.NodeEditor (getEdgeNodes, modifyNodeEditor)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection  (ConnectionId, connectionId)
import           Luna.Studio.React.Model.Node        (NodeId, nodeId)
import           Luna.Studio.React.Model.NodeEditor  (connections)
import           Luna.Studio.State.Global            (State, scene)


drawConnection :: API.Connection -> Command State Bool
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
redrawConnection connId = NodeEditor.getConnection connId >>=
    maybe (return False) (drawConnection . convert)

redrawConnections :: Command State ()
redrawConnections = do
    oldConnections <- NodeEditor.getConnections
    modifyNodeEditor $ connections .= def
    mapM_ (drawConnection . convert) oldConnections

redrawConnectionsForNode :: NodeId -> Command State [ConnectionId]
redrawConnectionsForNode nid = NodeEditor.getConnectionsContainingNode nid >>=
    (fmap . map) (view connectionId) . filterM (drawConnection . convert)

--TODO[LJK]: Should we remove all previous conns for nodes?
redrawConnectionsForNodes :: [NodeId] -> Command State [ConnectionId]
redrawConnectionsForNodes nodeIds = mapM NodeEditor.getConnectionsContainingNode nodeIds >>=
    (fmap . map) (view connectionId) . filterM (drawConnection . convert) . join


redrawConnectionsForEdgeNodes :: Command State [ConnectionId]
redrawConnectionsForEdgeNodes = getEdgeNodes >>=
    redrawConnectionsForNodes . (map (view nodeId))
