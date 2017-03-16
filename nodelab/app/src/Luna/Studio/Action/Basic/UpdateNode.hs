module Luna.Studio.Action.Basic.UpdateNode where

import           Control.Monad                           (filterM)
import           Empire.API.Data.Node                    (Node, NodeId, NodeTypecheckerUpdate, nodeId, tcNodeId, tcPorts)
import qualified Empire.API.Data.Node                    as Node
import           Empire.API.Data.Port                    (InPort (Self), PortId (InPortId))
import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNode)
import           Luna.Studio.Action.Command              (Command)
import qualified Luna.Studio.Action.State.Graph          as Graph
import           Luna.Studio.Action.State.Model          (shouldDisplayPortSelf)
import qualified Luna.Studio.Action.State.NodeEditor     as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node            (isSelected)
import qualified Luna.Studio.React.Model.Node            as Model
import           Luna.Studio.React.Model.Port            (visible)
import           Luna.Studio.State.Global                (State)


localUpdateNodes :: [Node] -> Command State ()
localUpdateNodes = mapM_ localUpdateNode

localUpdateNode :: Node -> Command State Bool
localUpdateNode node = Graph.inGraph (node ^. nodeId) >>= \exists ->
    if not exists
        then return False
        else do
            let nid       = node ^. nodeId
                nodeModel = convert node
            portSelfVis <- shouldDisplayPortSelf nodeModel
            selected    <- maybe False (view isSelected) <$> NodeEditor.getNode nid
            Graph.addNode node
            NodeEditor.addNode $ nodeModel & isSelected                                 .~ selected
                                           & Model.ports . ix (InPortId Self) . visible .~ portSelfVis
            void $ redrawConnectionsForNode nid
            return True

localUpdateNodeTypecheck :: NodeTypecheckerUpdate -> Command State Bool
localUpdateNodeTypecheck update = do
    mayNode <- Graph.getNode $ update ^. tcNodeId
    success <- flip (maybe (return False)) mayNode (\node ->
        localUpdateNode $ node & Node.ports .~ update ^. tcPorts) -- typecheck non-existing node?
    void . redrawConnectionsForNode $ update ^. tcNodeId
    return success

updateAllPortsSelfVisibility :: Command State ()
updateAllPortsSelfVisibility = do
    nids <- map (view Model.nodeId) <$> NodeEditor.getNodes
    void $ updatePortSelfVisibilityForIds nids

updatePortSelfVisibilityForIds :: [NodeId] -> Command State [NodeId]
updatePortSelfVisibilityForIds = filterM updatePortSelfVisibility

updatePortSelfVisibility :: NodeId -> Command State Bool
updatePortSelfVisibility nid = NodeEditor.getNode nid >>=
    maybe (return False) ( \node -> do
        vis <- shouldDisplayPortSelf node
        NodeEditor.modifyNode nid $ Model.ports . ix (InPortId Self) . visible .= vis
        return True
        )
