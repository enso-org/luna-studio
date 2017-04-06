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
import           Luna.Studio.React.Model.Node                (ExpressionNode, Node (Expression, Sidebar), NodeLoc, NodePath, SidebarNode,
                                                              nodeLoc, ports)
import           Luna.Studio.React.Model.Node.ExpressionNode (isSelected)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as Node
import qualified Luna.Studio.React.Model.Node.SidebarNode    as Sidebar
import           Luna.Studio.React.Model.Port                (Mode (Invisible), ensureVisibility, mode)
import           Luna.Studio.State.Global                    (State)

localUpdateNodes :: [Node] -> Command State ()
localUpdateNodes = mapM_ localUpdateNode

localUpdateExpressionNodes :: [ExpressionNode] -> Command State ()
localUpdateExpressionNodes = mapM_ localUpdateExpressionNode

localUpdateSidebarNodes :: [SidebarNode] -> Command State ()
localUpdateSidebarNodes = mapM_ localUpdateSidebarNode

localUpdateNode :: Node -> Command State Bool
localUpdateNode (Expression node) = localUpdateExpressionNode node
localUpdateNode (Sidebar    node) = localUpdateSidebarNode node

localUpdateSidebarNode :: SidebarNode -> Command State Bool
localUpdateSidebarNode node = NodeEditor.getSidebarNode (node ^. nodeLoc) >>= \mayNode ->
    case mayNode of
        Nothing       -> return False
        Just prevNode -> do
            let sidebarMode = prevNode ^. Sidebar.mode
            NodeEditor.addSidebarNode $ node & Sidebar.mode .~ sidebarMode
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

localUpdateNodeTypecheck :: NodePath -> NodeTypecheckerUpdate -> Command State Bool
localUpdateNodeTypecheck path update = do
    let tcNodeLoc = convert (path, update ^. tcNodeId)
    mayNode <- NodeEditor.getNode tcNodeLoc
    success <- flip (maybe (return False)) mayNode (\node ->
        localUpdateNode $ node & ports .~ (convert <$> (update ^. tcPorts))) -- typecheck non-existing node?
    void $ redrawConnectionsForNode tcNodeLoc
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
