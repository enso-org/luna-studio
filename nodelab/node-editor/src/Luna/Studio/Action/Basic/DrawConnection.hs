module Luna.Studio.Action.Basic.DrawConnection where

import           Empire.API.Data.PortRef             (InPortRef, OutPortRef)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.Model      (createConnectionModel)
import           Luna.Studio.Action.State.NodeEditor (getConnections, getInputNodes, getOutputNodes, getScene, modifyNodeEditor)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Prelude
import           Luna.Studio.React.Model.Connection  (Connection, ConnectionId, connectionId, dst, src, toConnectionsMap)
import           Luna.Studio.React.Model.Node        (NodeLoc, nodeLoc)
import           Luna.Studio.React.Model.NodeEditor  (connections)
import           Luna.Studio.React.Model.Scene       (inputSidebar, outputSidebar)
import           Luna.Studio.State.Global            (State)


createConnection :: OutPortRef -> InPortRef -> Command State (Maybe Connection)
createConnection src' dst' = do
    prevInputSidebar  <- view inputSidebar  `fmap2` getScene
    prevOutputSidebar <- view outputSidebar `fmap2` getScene
    mayConnModel      <- createConnectionModel src' dst'
    newInputSidebar   <- view inputSidebar  `fmap2` getScene
    newOutputSidebar  <- view outputSidebar `fmap2` getScene
    when (newInputSidebar /= prevInputSidebar || newOutputSidebar /= prevOutputSidebar) $
        void redrawConnectionsForSidebarNodes
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
redrawConnectionsForNode :: NodeLoc -> Command State [ConnectionId]
redrawConnectionsForNode nl = do
    oldConnections <- NodeEditor.getConnectionsContainingNode nl
    conns <- catMaybes <$> mapM (\conn -> createConnection (conn ^. src) (conn ^. dst)) oldConnections
    mapM_ NodeEditor.addConnection conns
    return $ map (view connectionId) conns

--TODO[LJK]: Should we remove all previous conns for nodes?
redrawConnectionsForNodes :: [NodeLoc] -> Command State [ConnectionId]
redrawConnectionsForNodes nodeLocs = do
    oldConnections <- NodeEditor.getConnectionsContainingNodes nodeLocs
    conns <- catMaybes <$> mapM (\conn -> createConnection (conn ^. src) (conn ^. dst)) oldConnections
    mapM_ NodeEditor.addConnection conns
    return $ map (view connectionId) conns

redrawConnectionsForSidebarNodes :: Command State [ConnectionId]
redrawConnectionsForSidebarNodes = do
    ins  <- redrawConnectionsForNodes . map (view nodeLoc) =<< getInputNodes
    outs <- redrawConnectionsForNodes . map (view nodeLoc) =<< getOutputNodes
    return $ ins <> outs
