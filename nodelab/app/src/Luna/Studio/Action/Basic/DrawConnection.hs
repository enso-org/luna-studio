module Luna.Studio.Action.Basic.DrawConnection where

import           Empire.API.Data.PortRef             (InPortRef, OutPortRef)
import           JS.Scene                            (inputSidebar, outputSidebar)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.Model      (createConnectionModel)
import           Luna.Studio.Action.State.NodeEditor (getConnections, getEdgeNodes, modifyNodeEditor)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection  (Connection, ConnectionId, connectionId, dst, src, toConnectionsMap)
import           Luna.Studio.React.Model.Node        (NodeId, nodeId)
import           Luna.Studio.React.Model.NodeEditor  (connections)
import           Luna.Studio.State.Global            (State, scene)


createConnection :: OutPortRef -> InPortRef -> Command State (Maybe Connection)
createConnection src' dst' = do
    prevInputSidebar  <- (fmap . fmap) (view inputSidebar)  $ use scene
    prevOutputSidebar <- (fmap . fmap) (view outputSidebar) $ use scene
    mayConnModel      <- createConnectionModel src' dst'
    newInputSidebar   <- (fmap . fmap) (view inputSidebar)  $ use scene
    newOutputSidebar  <- (fmap . fmap) (view outputSidebar) $ use scene
    when (newInputSidebar /= prevInputSidebar || newOutputSidebar /= prevOutputSidebar) $
        void redrawConnectionsForEdgeNodes
    return $ mayConnModel

redrawConnection :: ConnectionId -> Command State Bool
redrawConnection connId = do
    mayConn <- NodeEditor.getConnection connId
    case mayConn of
        Just conn -> do
            mayNewConn <- createConnection (conn ^. src) (conn ^. dst)
            withJust mayNewConn NodeEditor.addConnection
            return $ isJust mayNewConn
        Nothing -> return False

redrawConnections :: Command State ()
redrawConnections = do
    oldConnections <- getConnections
    conns <- catMaybes <$> mapM (\conn -> createConnection (conn ^. src) (conn ^. dst)) oldConnections
    modifyNodeEditor $ connections .= toConnectionsMap conns

--TODO[LJK]: Should we remove all previous conns for node?
redrawConnectionsForNode :: NodeId -> Command State [ConnectionId]
redrawConnectionsForNode nid = do
    oldConnections <- NodeEditor.getConnectionsContainingNode nid
    conns <- catMaybes <$> mapM (\conn -> createConnection (conn ^. src) (conn ^. dst)) oldConnections
    mapM_ NodeEditor.addConnection conns
    return $ map (view connectionId) conns

--TODO[LJK]: Should we remove all previous conns for nodes?
redrawConnectionsForNodes :: [NodeId] -> Command State [ConnectionId]
redrawConnectionsForNodes nodeIds = do
    oldConnections <- NodeEditor.getConnectionsContainingNodes nodeIds
    conns <- catMaybes <$> mapM (\conn -> createConnection (conn ^. src) (conn ^. dst)) oldConnections
    mapM_ NodeEditor.addConnection conns
    return $ map (view connectionId) conns

redrawConnectionsForEdgeNodes :: Command State [ConnectionId]
redrawConnectionsForEdgeNodes = getEdgeNodes >>=
    redrawConnectionsForNodes . map (view nodeId)
