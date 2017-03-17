module Luna.Studio.Action.Basic.UpdateNode where

import           Control.Monad                           (filterM)
import           Empire.API.Data.Node                    (NodeTypecheckerUpdate, tcNodeId, tcPorts)
import           Empire.API.Data.Port                    (InPort (Self), PortId (InPortId))
import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNode)
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.State.Model          (shouldDisplayPortSelf)
import qualified Luna.Studio.Action.State.NodeEditor     as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node            (Node, NodeId, isSelected)
import qualified Luna.Studio.React.Model.Node            as Node
import           Luna.Studio.React.Model.Port            (visible)
import           Luna.Studio.State.Global                (State)


localUpdateNodes :: [Node] -> Command State ()
localUpdateNodes = mapM_ localUpdateNode

localUpdateNode :: Node -> Command State Bool
localUpdateNode node = NodeEditor.inGraph (node ^. Node.nodeId) >>= \exists ->
    if not exists
        then return False
        else do
            let nid       = node ^. Node.nodeId
            portSelfVis <- shouldDisplayPortSelf node
            selected    <- maybe False (view isSelected) <$> NodeEditor.getNode nid
            NodeEditor.addNode $ node & isSelected                                .~ selected
                                      & Node.ports . ix (InPortId Self) . visible .~ portSelfVis
            void $ redrawConnectionsForNode nid
            return True

localUpdateNodeTypecheck :: NodeTypecheckerUpdate -> Command State Bool
localUpdateNodeTypecheck update = do
    mayNode <- NodeEditor.getNode $ update ^. tcNodeId
    success <- flip (maybe (return False)) mayNode (\node ->
        localUpdateNode $ node & Node.ports .~ (convert <$> (update ^. tcPorts))) -- typecheck non-existing node?
    void . redrawConnectionsForNode $ update ^. tcNodeId
    return success

updateAllPortsSelfVisibility :: Command State ()
updateAllPortsSelfVisibility = do
    nids <- map (view Node.nodeId) <$> NodeEditor.getNodes
    void $ updatePortSelfVisibilityForIds nids

updatePortSelfVisibilityForIds :: [NodeId] -> Command State [NodeId]
updatePortSelfVisibilityForIds = filterM updatePortSelfVisibility

updatePortSelfVisibility :: NodeId -> Command State Bool
updatePortSelfVisibility nid = NodeEditor.getNode nid >>=
    maybe (return False) ( \node -> do
        vis <- shouldDisplayPortSelf node
        NodeEditor.modifyNode nid $ Node.ports . ix (InPortId Self) . visible .= vis
        return True
        )
