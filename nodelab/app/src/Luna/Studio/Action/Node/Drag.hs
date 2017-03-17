{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Node.Drag
    ( startNodeDrag
    , nodesDrag
    , handleNodeDragMouseUp
    ) where

import           Control.Arrow
import qualified Data.Map                            as Map
import           Data.Position                       (Position, move, vector)
import           Empire.API.Data.Connection          (Connection (Connection))
import qualified Empire.API.Data.Connection          as Connection
import           Empire.API.Data.Node                (NodeId)
import           Empire.API.Data.Port                (InPort (Self), OutPort (All), PortId (InPortId))
import           Empire.API.Data.PortRef             (InPortRef (InPortRef), OutPortRef (OutPortRef))
import           Luna.Studio.Action.Basic            (connect, localMoveNodes, moveNodes, selectNodes, updatePortSelfVisibility)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.Node.Snap        (snap)
import           Luna.Studio.Action.State.Model      (createConnectionModel, getIntersectingConnections)
import           Luna.Studio.Action.State.NodeEditor (getSelectedNodes, modifyNodeEditor)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Event.Mouse             (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node        (isSelected)
import qualified Luna.Studio.React.Model.Node        as Model
import           Luna.Studio.React.Model.NodeEditor  (currentConnections)
import           Luna.Studio.React.Model.Port        as Port
import           Luna.Studio.State.Action            (Action (begin, continue, end, update), NodeDrag (NodeDrag), nodeDragAction,
                                                      nodeDragNodeId, nodeDragNodesStartPos, nodeDragSnappedConn, nodeDragStartPos)

import           Luna.Studio.Action.State.Action     (beginActionWithKey, continueActionWithKey, removeActionFromState, updateActionWithKey)
import qualified Luna.Studio.Action.State.Graph      as Graph
import           Luna.Studio.State.Global            (State)
import           React.Flux                          (MouseEvent)


instance Action (Command State) NodeDrag where
    begin        = beginActionWithKey    nodeDragAction
    continue     = continueActionWithKey nodeDragAction
    update       = updateActionWithKey   nodeDragAction
    end nodeDrag = do
            metaUpdate <- map (view Model.nodeId &&& view Model.position) <$> getSelectedNodes
            moveNodes metaUpdate
            clearSnappedConnection nodeDrag
            removeActionFromState nodeDragAction


startNodeDrag :: Position -> NodeId -> Bool -> Command State ()
startNodeDrag coord nid snapped = do
    mayNode <- NodeEditor.getNode nid
    withJust mayNode $ \node -> do
        unless (node ^. isSelected) $ selectNodes [nid]
        nodes <- getSelectedNodes
        let nodesPos = Map.fromList $ (view Model.nodeId &&& view Model.position) <$> nodes
        if snapped then do
            let snappedNodes = Map.map snap nodesPos
            begin $ NodeDrag coord nid snappedNodes Nothing
            void . localMoveNodes $ Map.toList snappedNodes
        else begin $ NodeDrag coord nid nodesPos Nothing

nodesDrag :: MouseEvent -> Bool -> NodeDrag -> Command State ()
nodesDrag evt snapped nodeDrag = do
    coord <- workspacePosition evt
    let mouseStartPos = view nodeDragStartPos      nodeDrag
        draggedNodeId = view nodeDragNodeId        nodeDrag
        nodesStartPos = view nodeDragNodesStartPos nodeDrag
        delta = coord ^. vector - mouseStartPos ^. vector
        shift' = if snapped then do
                     case Map.lookup draggedNodeId nodesStartPos of
                         Just pos -> snap (move delta pos) ^. vector - pos ^. vector
                         Nothing  -> delta
                 else delta
    moveNodes . Map.toList $ Map.map (move shift') nodesStartPos
    snapConnectionsForNodes coord $ Map.keys nodesStartPos

clearSnappedConnection :: NodeDrag -> Command State ()
clearSnappedConnection nodeDrag = do
    let nid = nodeDrag ^. nodeDragNodeId
    modifyNodeEditor $ currentConnections .= def
    void $ updatePortSelfVisibility nid
    continue $ \nodeDrag' -> do
        update $ nodeDrag' & nodeDragSnappedConn .~ Nothing

snapConnectionsForNodes :: Position -> [NodeId] -> Command State ()
snapConnectionsForNodes mousePos nodeIds = when (length nodeIds == 1) $ forM_ nodeIds $ \nid -> do
    mayNode <- NodeEditor.getNode nid
    withJust mayNode $ \node -> do
        mayConnId <- getIntersectingConnections node mousePos
        case mayConnId of
            Just connId -> do
                let selfPortRef = InPortRef  nid Self
                    outPortRef  = OutPortRef nid All
                mayConn       <- Graph.getConnection connId
                NodeEditor.modifyNode nid $ Model.ports . ix (InPortId Self) . Port.visible .= True
                mayConnModel1 <- fmap join $ mapM createConnectionModel $ flip Connection selfPortRef <$> view Connection.src <$> mayConn
                mayConnModel2 <- fmap join $ mapM createConnectionModel $      Connection outPortRef  <$> view Connection.dst <$> mayConn
                case (,) <$> mayConnModel1 <*> mayConnModel2 of
                    Just (connModel1, connModel2) -> do
                        modifyNodeEditor $ currentConnections .= map convert [connModel1, connModel2]
                        continue $ \nodeDrag -> update $ nodeDrag & nodeDragSnappedConn ?~ connId
                    _ -> continue clearSnappedConnection
            _ -> continue clearSnappedConnection

handleNodeDragMouseUp :: MouseEvent -> NodeDrag -> Command State ()
handleNodeDragMouseUp evt nodeDrag = do
    coord <- workspacePosition evt
    let startPos = view nodeDragStartPos nodeDrag
        nid      = view nodeDragNodeId   nodeDrag
    if startPos == coord then
        selectNodes [nid]
    else do
        metaUpdate <- map (view Model.nodeId &&& view Model.position) <$> getSelectedNodes
        moveNodes metaUpdate
        withJust (nodeDrag ^. nodeDragSnappedConn) $ \connId -> do
            mayConn <- Graph.getConnection connId
            withJust mayConn $ \conn -> do
                connect (Left $ conn ^. Connection.src) $ Right nid
                connect (Right nid)                     $ Left $ conn ^. Connection.dst
        clearSnappedConnection nodeDrag
    continue stopNodeDrag


stopNodeDrag :: NodeDrag -> Command State ()
stopNodeDrag nodeDrag = do
    clearSnappedConnection nodeDrag
    removeActionFromState nodeDragAction
