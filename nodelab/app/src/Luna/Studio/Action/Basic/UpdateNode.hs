module Luna.Studio.Action.Basic.UpdateNode where

import           Control.Monad                               (filterM)
import           Empire.API.Data.Node                        (NodeTypecheckerUpdate, tcNodeId, tcPorts)
import           Empire.API.Data.Port                        (InPort (Self), PortId (InPortId))
import           Luna.Studio.Action.Basic.DrawConnection     (redrawConnectionsForNode)
import           Luna.Studio.Action.Basic.Scene              (updateScene)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.Model              (shouldDisplayPortSelf)
import qualified Luna.Studio.Action.State.NodeEditor         as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node                (EdgeNode, ExpressionNode, Node (Edge, Expression), NodeLoc, nodeLoc, ports)
import qualified Luna.Studio.React.Model.Node.EdgeNode       as Edge
import           Luna.Studio.React.Model.Node.ExpressionNode (isSelected)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as Node
import           Luna.Studio.React.Model.Port                (Mode (Invisible), ensureVisibility, mode)
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
localUpdateEdgeNode node = NodeEditor.getEdgeNode (node ^. nodeLoc) >>= \mayNode ->
    case mayNode of
        Nothing       -> return False
        Just prevNode -> do
            let edgeMode = prevNode ^. Edge.mode
            NodeEditor.addEdgeNode $ node & Edge.mode .~ edgeMode
            updateScene
            return True

localUpdateExpressionNode :: ExpressionNode -> Command State Bool
localUpdateExpressionNode node = NodeEditor.getExpressionNode (node ^. nodeLoc) >>= \mayNode ->
    case mayNode of
        Nothing       -> return False
        Just prevNode -> do
            let selected = prevNode ^. isSelected
                mode'    = prevNode ^. Node.mode
            portSelfVis <- shouldDisplayPortSelf node
            let (selfMode :: Mode -> Mode) = if portSelfVis then ensureVisibility else const Invisible
            NodeEditor.addExpressionNode $ node & isSelected                        .~ selected
                                                & ports . ix (InPortId Self) . mode %~ selfMode
                                                & Node.mode                         .~ mode'
            void . redrawConnectionsForNode $ node ^. nodeLoc
            return True

localUpdateNodeTypecheck :: NodeTypecheckerUpdate -> Command State Bool
localUpdateNodeTypecheck update = do
    mayNode <- NodeEditor.getNode $ convert $ update ^. tcNodeId
    success <- flip (maybe (return False)) mayNode (\node ->
        localUpdateNode $ node & ports .~ (convert <$> (update ^. tcPorts))) -- typecheck non-existing node?
    void . redrawConnectionsForNode $ convert $ update ^. tcNodeId
    return success

updateAllPortsSelfVisibility :: Command State ()
updateAllPortsSelfVisibility = do
    nls <- map (view nodeLoc) <$> NodeEditor.getExpressionNodes
    void $ updatePortSelfVisibilityForIds nls

updatePortSelfVisibilityForIds :: [NodeLoc] -> Command State [NodeLoc]
updatePortSelfVisibilityForIds = filterM updatePortSelfVisibility

updatePortSelfVisibility :: NodeLoc -> Command State Bool
updatePortSelfVisibility nid = NodeEditor.getExpressionNode nid >>=
    maybe (return False) ( \node -> do
        vis <- shouldDisplayPortSelf node
        NodeEditor.modifyExpressionNode nid $ ports . ix (InPortId Self) . mode %= if vis then ensureVisibility else const Invisible
        return True
        )
