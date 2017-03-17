module Luna.Studio.Action.State.Graph where

import qualified Data.HashMap.Strict           as Map
import qualified Data.Set                      as Set

import           Empire.API.Data.Connection    (Connection, ConnectionId, connectionId, containsNode, containsPortRef, dstNodeId, srcNodeId)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.PortRef       (AnyPortRef)
import           Luna.Studio.Action.Command    (Command)
import           Luna.Studio.Batch.Workspace   (currentLocation, isGraphLoaded)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global      (State, graph, workspace)
import           Luna.Studio.State.Graph       (ConnectionsMap, Graph, connectionsMap)


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
