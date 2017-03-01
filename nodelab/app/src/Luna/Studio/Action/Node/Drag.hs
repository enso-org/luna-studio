{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Node.Drag
    ( startNodeDrag
    , nodesDrag
    , handleNodeDragMouseUp
    ) where

import           Control.Arrow
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Position                          (Position, move, toTuple, vector)
import           Empire.API.Data.Connection             (Connection (Connection))
import qualified Empire.API.Data.Connection             as Connection
import           Empire.API.Data.Node                   (NodeId)
import qualified Empire.API.Data.Node                   as Node
import           Empire.API.Data.Port                   (InPort (Self), OutPort (All), PortId (InPortId))
import           Empire.API.Data.PortRef                (InPortRef (InPortRef), OutPortRef (OutPortRef))
import qualified Empire.API.Data.PortRef                as PortRef
import           Luna.Studio.Action.Batch               (autoconnect)
import qualified Luna.Studio.Action.Batch               as BatchCmd
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.Geometry.Connection (createConnectionModel)
import           Luna.Studio.Action.Geometry.Node       (getIntersectingConnections)
import           Luna.Studio.Action.Graph.Selection     (selectNodes, selectedNodes)
import           Luna.Studio.Action.Graph.Update        (updateConnectionsForNodes)
import           Luna.Studio.Action.Node.Snap           (snap)
import           Luna.Studio.Event.Mouse                (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection     (toCurrentConnection)
import qualified Luna.Studio.React.Model.Node           as Model
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import           Luna.Studio.React.Model.Port           as Port
import           Luna.Studio.State.Action               (Action (begin, continue, end, update), NodeDrag (NodeDrag), nodeDragAction)
import qualified Luna.Studio.State.Action               as Action
import           Luna.Studio.State.Global               (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                         updateActionWithKey)
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph
import           React.Flux                             (MouseEvent)


instance Action (Command State) NodeDrag where
    begin    = beginActionWithKey    nodeDragAction
    continue = continueActionWithKey nodeDragAction
    update   = updateActionWithKey   nodeDragAction
    end _    = updateMovedNodes >> removeActionFromState nodeDragAction

startNodeDrag :: Position -> NodeId -> Bool -> Command State ()
startNodeDrag coord nodeId snapped = do
    mayNode <- Global.getNode nodeId
    withJust mayNode $ \node -> do
        let isSelected = node ^. Model.isSelected
        unless isSelected $ selectNodes [nodeId]
        nodes <- selectedNodes
        let nodesPos = Map.fromList $ (view Model.nodeId &&& view Model.position) <$> nodes
        if snapped then do
            let snappedNodes = Map.map snap nodesPos
            begin $ NodeDrag coord nodeId snappedNodes Nothing
            moveNodes snappedNodes
        else begin $ NodeDrag coord nodeId nodesPos Nothing

nodesDrag :: MouseEvent -> Bool -> NodeDrag -> Command State ()
nodesDrag evt snapped nodeDrag = do
    coord <- workspacePosition evt
    let mouseStartPos = view Action.nodeDragStartPos      nodeDrag
        draggedNodeId = view Action.nodeDragNodeId        nodeDrag
        nodesStartPos = view Action.nodeDragNodesStartPos nodeDrag
        delta = coord ^. vector - mouseStartPos ^. vector
        shift' = if snapped then do
                     case Map.lookup draggedNodeId nodesStartPos of
                         Just pos -> snap (move delta pos) ^. vector - pos ^. vector
                         Nothing  -> delta
                 else delta
    moveNodes $ Map.map (move shift') nodesStartPos
    snapConnectionsForNodes coord $ Map.keys nodesStartPos

clearSnappedConnection :: NodeDrag -> Command State ()
clearSnappedConnection nodeDrag = do
    let nodeId = nodeDrag ^. Action.nodeDragNodeId
    Global.modifyNodeEditor $ NodeEditor.currentConnections .= def
    whenM (isNothing <$> Global.getConnection (InPortRef nodeId Self)) $
        Global.modifyNode nodeId $ Model.ports . at (InPortId Self) . _Just . Port.visible .= False
    continue $ \nodeDrag' -> do
        update $ nodeDrag' & Action.nodeDragSnappedConn .~ Nothing

snapConnectionsForNodes :: Position -> [NodeId] -> Command State ()
snapConnectionsForNodes mousePos nodeIds = when (length nodeIds == 1) $ forM_ nodeIds $ \nodeId -> do
    mayNode <- Global.getNode nodeId
    withJust mayNode $ \node -> do
        let nodePos = if node ^. Model.mode == Model.Expanded then mousePos else node ^. Model.position
        connIds <- getIntersectingConnections nodeId nodePos
        if length connIds == 1 then forM_ connIds $ \connId -> do
            let selfPortRef = InPortRef  nodeId Self
                outPortRef  = OutPortRef nodeId All
            mayConn       <- use $ Global.graph . Graph.connectionsMap . at connId
            Global.modifyNode nodeId $ Model.ports . at (InPortId Self) . _Just . Port.visible .= True
            mayConnModel1 <- fmap join $ mapM createConnectionModel $ flip Connection selfPortRef <$> view Connection.src <$> mayConn
            mayConnModel2 <- fmap join $ mapM createConnectionModel $      Connection outPortRef  <$> view Connection.dst <$> mayConn
            case (,) <$> mayConnModel1 <*> mayConnModel2 of
                Just (connModel1, connModel2) -> do
                    Global.modifyNodeEditor $ NodeEditor.currentConnections .= map toCurrentConnection [connModel1, connModel2]
                    continue $ \nodeDrag -> update $ nodeDrag & Action.nodeDragSnappedConn ?~ connId
                _ -> continue clearSnappedConnection
        else continue clearSnappedConnection

moveNodes :: Map NodeId Position -> Command State ()
moveNodes nodesPos = do
    Global.modifyNodeEditor $ forM_ (Map.toList nodesPos) $ \(nodeId, pos) ->
        NodeEditor.nodes . at nodeId %= fmap (Model.position .~ pos)
    updateConnectionsForNodes $ Map.keys nodesPos

updateMovedNodes :: Command State ()
updateMovedNodes = do
    selected <- selectedNodes
    let nodesToUpdate = (view Model.nodeId &&& view Model.position) <$> selected
    updates <- forM nodesToUpdate $ \(wid, pos) -> do
        Global.graph . Graph.nodesMap . ix wid . Node.position .= toTuple (pos ^. vector)
        newMeta <- preuse $ Global.graph . Graph.nodesMap . ix wid . Node.nodeMeta
        return $ (wid, ) <$> newMeta
    BatchCmd.updateNodeMeta $ catMaybes updates
    updateConnectionsForNodes $ fst <$> nodesToUpdate

handleNodeDragMouseUp :: MouseEvent -> NodeDrag -> Command State ()
handleNodeDragMouseUp evt nodeDrag = do
    coord <- workspacePosition evt
    let startPos = view Action.nodeDragStartPos nodeDrag
        nodeId   = view Action.nodeDragNodeId   nodeDrag
    if startPos == coord then
        selectNodes [nodeId]
    else do
        updateMovedNodes
        withJust (nodeDrag ^. Action.nodeDragSnappedConn) $ \connId -> do
            mayConn <- use $ Global.graph . Graph.connectionsMap . at connId
            withJust mayConn $ \conn -> do
                autoconnect (conn ^. Connection.src . PortRef.srcNodeId) nodeId
                autoconnect nodeId (conn ^. Connection.dst . PortRef.dstNodeId)
    continue stopNodeDrag


stopNodeDrag :: NodeDrag -> Command State ()
stopNodeDrag nodeDrag = do
    clearSnappedConnection nodeDrag
    removeActionFromState nodeDragAction
