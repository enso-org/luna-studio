module Luna.Studio.Action.Basic.UpdateNode where

import           Control.Monad                               (filterM)
import           Empire.API.Data.Node                        (NodeTypecheckerUpdate, tcNodeId, tcPorts)
import           Empire.API.Data.Port                        (InPort (Self), PortId (InPortId))
import           Luna.Studio.Action.Basic.DrawConnection     (redrawConnectionsForNode)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.Model              (shouldDisplayPortSelf)
import qualified Luna.Studio.Action.State.NodeEditor         as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node                (EdgeNode, ExpressionNode, Node (Edge, Expression), NodeId, nodeId, ports)
import           Luna.Studio.React.Model.Node.ExpressionNode (isSelected)
import           Luna.Studio.React.Model.Port                (visible)
import           Luna.Studio.State.Global                    (State)

localUpdateNodes :: [Node] -> Command State ()
localUpdateNodes = mapM_ localUpdateNode

localUpdateExpressionNodes :: [ExpressionNode] -> Command State ()
localUpdateExpressionNodes = mapM_ localUpdateExpressionNode

localUpdateEdgeNodes :: [EdgeNode] -> Command State ()
localUpdateEdgeNodes = mapM_ localUpdateEdgeNode

localUpdateNode :: Node -> Command State Bool
localUpdateNode (Expression node) = localUpdateExpressionNode node
localUpdateNode (Edge edge)       = localUpdateEdgeNode edge

localUpdateEdgeNode :: EdgeNode -> Command State Bool
localUpdateEdgeNode node = NodeEditor.inGraph (node ^. nodeId) >>= \exists ->
    if not exists
        then return False
        else do
            let nid = node ^. nodeId
            NodeEditor.addEdgeNode node
            void $ redrawConnectionsForNode nid
            return True

localUpdateExpressionNode :: ExpressionNode -> Command State Bool
localUpdateExpressionNode node = NodeEditor.getExpressionNode (node ^. nodeId) >>= \mayNode ->
    case mayNode of
        Nothing       -> return False
        Just prevNode -> do
            let selected = prevNode ^. isSelected
            portSelfVis <- shouldDisplayPortSelf node
            NodeEditor.addExpressionNode $ node & isSelected                           .~ selected
                                                & ports . ix (InPortId Self) . visible .~ portSelfVis
            void . redrawConnectionsForNode $ node ^. nodeId
            return True

localUpdateNodeTypecheck :: NodeTypecheckerUpdate -> Command State Bool
localUpdateNodeTypecheck update = do
    mayNode <- NodeEditor.getNode $ update ^. tcNodeId
    success <- flip (maybe (return False)) mayNode (\node ->
        localUpdateNode $ node & ports .~ (convert <$> (update ^. tcPorts))) -- typecheck non-existing node?
    void . redrawConnectionsForNode $ update ^. tcNodeId
    return success

updateAllPortsSelfVisibility :: Command State ()
updateAllPortsSelfVisibility = do
    nids <- map (view nodeId) <$> NodeEditor.getExpressionNodes
    void $ updatePortSelfVisibilityForIds nids

updatePortSelfVisibilityForIds :: [NodeId] -> Command State [NodeId]
updatePortSelfVisibilityForIds = filterM updatePortSelfVisibility

updatePortSelfVisibility :: NodeId -> Command State Bool
updatePortSelfVisibility nid = NodeEditor.getExpressionNode nid >>=
    maybe (return False) ( \node -> do
        vis <- shouldDisplayPortSelf node
        NodeEditor.modifyExpressionNode nid $ ports . ix (InPortId Self) . visible .= vis
        return True
        )
