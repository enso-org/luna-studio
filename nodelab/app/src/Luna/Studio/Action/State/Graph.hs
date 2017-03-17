module Luna.Studio.Action.State.Graph where

import           Control.Monad.Trans.Maybe     (runMaybeT)
import qualified Data.HashMap.Strict           as Map
import qualified Data.Set                      as Set
import           Empire.API.Data.Connection    (Connection, ConnectionId, connectionId, containsNode, containsPortRef, dstNodeId, srcNodeId)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node, NodeId, isEdge, nodeId, ports)
import           Empire.API.Data.Port          (Port)
import           Empire.API.Data.PortRef       (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef       as PortRef
import           Luna.Studio.Action.Command    (Command)
import           Luna.Studio.Batch.Workspace   (currentLocation, isGraphLoaded)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global      (State, graph, workspace)
import           Luna.Studio.State.Graph       (ConnectionsMap, Graph (Graph), NodesMap, connectionsMap, nodesMap)


isCurrentLocation :: GraphLocation -> Command State Bool
isCurrentLocation location = uses (workspace . currentLocation) (== location)

isCurrentLocationAndGraphLoaded :: GraphLocation -> Command State Bool
isCurrentLocationAndGraphLoaded location = do
    icl <- isCurrentLocation location
    igl <- use $ workspace . isGraphLoaded
    return $ icl && igl


getGraph :: Command State Graph
getGraph = use graph

modifyGraph :: (Graph -> Graph) -> Command State ()
modifyGraph f = graph %= f

separateSubgraph :: [NodeId] -> Command State Graph
separateSubgraph nodeIds = do
    let idSet = Set.fromList nodeIds
        inSet = flip Set.member idSet
    nodes <- Map.filterWithKey (inSet .: const)  <$> getNodesMap
    conns <- Map.filter (inSet . view dstNodeId) <$> getConnectionsMap
    return $ Graph nodes conns


addNode :: Node -> Command State ()
addNode node = modifyGraph $ nodesMap . at (node ^. nodeId) ?~ node

getAllNodes :: Command State [Node]
getAllNodes = Map.elems <$> getNodesMap

getEdgeNodes :: Command State [Node]
getEdgeNodes = filter isEdge <$> getAllNodes

getNode :: NodeId -> Command State (Maybe Node)
getNode nid = Map.lookup nid <$> getNodesMap

getNodes :: Command State [Node]
getNodes = filter (not . isEdge) <$> getAllNodes

getNodesMap :: Command State NodesMap
getNodesMap = view nodesMap <$> getGraph

modifyNode :: NodeId -> (Node -> Node) -> Command State ()
modifyNode nid f = modifyGraph $ nodesMap . ix nid %~ f

removeNode :: NodeId -> Command State ()
removeNode nid = modifyGraph $ nodesMap . at nid .~ Nothing

updateNodes :: NodesMap -> Command State ()
updateNodes update = modifyGraph $ nodesMap .~ update


addConnection :: Connection -> Command State ()
addConnection conn = modifyGraph $ connectionsMap . at (conn ^. connectionId) ?~ conn

getConnection :: ConnectionId -> Command State (Maybe Connection)
getConnection connId = Map.lookup connId <$> getConnectionsMap

getConnections :: Command State [Connection]
getConnections = Map.elems <$> getConnectionsMap

getConnectionsMap :: Command State ConnectionsMap
getConnectionsMap = view connectionsMap <$> getGraph

getConnectionsBetweenNodes :: NodeId -> NodeId -> Command State [Connection]
getConnectionsBetweenNodes nid1 nid2 =
    filter (\conn -> containsNode nid1 conn && containsNode nid2 conn) <$> getConnections

getConnectionsContainingNode :: NodeId -> Command State [Connection]
getConnectionsContainingNode nid = filter (containsNode nid) <$> getConnections

getConnectionsContainingNodes :: [NodeId] -> Command State [Connection]
getConnectionsContainingNodes nodeIds = filter containsNode' <$> getConnections where
    nodeIdsSet         = Set.fromList nodeIds
    containsNode' conn = Set.member (conn ^. srcNodeId) nodeIdsSet
                      || Set.member (conn ^. dstNodeId) nodeIdsSet

getConnectionsContainingPortRef :: AnyPortRef -> Command State [Connection]
getConnectionsContainingPortRef portRef = filter (containsPortRef portRef) <$> getConnections

getConnectionsToNode :: NodeId -> Command State [Connection]
getConnectionsToNode nid = filter (\conn -> conn ^. dstNodeId == nid) <$> getConnections

modifyConnection :: ConnectionId -> (Connection -> Connection) -> Command State ()
modifyConnection connId f = modifyGraph $ connectionsMap . ix connId %~ f

removeConnection :: ConnectionId -> Command State ()
removeConnection connId = modifyGraph $ connectionsMap . at connId .~ Nothing

updateConnections :: ConnectionsMap -> Command State ()
updateConnections update = modifyGraph $ connectionsMap .~ update


class GraphElementId a where
    inGraph :: a -> Command State Bool
instance GraphElementId NodeId where
    inGraph = fmap isJust . getNode
instance GraphElementId ConnectionId where
    inGraph = fmap isJust . getConnection

class HasPort a where
    getPort :: a -> Command State (Maybe Port)
instance HasPort InPortRef where
    getPort portRef = getPortFromAnyPortRef $ InPortRef' portRef
instance HasPort OutPortRef where
    getPort portRef = getPortFromAnyPortRef $ OutPortRef' portRef
instance HasPort AnyPortRef where
    getPort = getPortFromAnyPortRef

getPortFromAnyPortRef :: AnyPortRef -> Command State (Maybe Port)
getPortFromAnyPortRef portRef = runMaybeT $ do
    Just node <- lift $ getNode $ portRef ^. PortRef.nodeId
    fromJustM $ node ^? ports . ix (portRef ^. PortRef.portId)
