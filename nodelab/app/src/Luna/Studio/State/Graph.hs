{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.State.Graph
    ( State(..)
    , addConnection
    , addNode
    , connectionIdsContainingNode
    , connections
    , connectionsContainingNode
    , connectionsContainingNodes
    , connectionsMap
    , connectionsToNodes
    , connectionToNodeIds
    , connectionsToNodesIds
    , getConnectionNodeIds
    , getConnections
    , getConnectionsMap
    , getNodes
    , getNodesMap
    , hasConnections
    , lookUpConnection
    , nodes
    , nodesMap
    , removeConnections
    , removeNode
    , updateNodes
    ) where

import           Luna.Studio.Prelude        hiding ((.=))

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Set                   as Set

import           Data.Aeson                 hiding ((.:))
import           Empire.API.Data.Connection (Connection (..), ConnectionId)
import qualified Empire.API.Data.Connection as Connection
import           Empire.API.Data.Node       (Node, NodeId)
import qualified Empire.API.Data.Node       as Node
import           Empire.API.Data.PortRef    (InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef    as PortRef
import           Luna.Studio.Action.Command (Command)


type NodesMap       = HashMap NodeId Node
type ConnectionsMap = HashMap ConnectionId Connection

data State = State { _nodesMap             :: NodesMap
                   , _connectionsMap       :: ConnectionsMap
                   } deriving (Show, Eq, Generic)

makeLenses ''State

instance ToJSON State
instance Default State where
    def = State def def

connectionToNodeIds :: Connection -> (NodeId, NodeId)
connectionToNodeIds conn = ( conn ^. Connection.src . PortRef.srcNodeId
                           , conn ^. Connection.dst . PortRef.dstNodeId)

nodes :: Getter State [Node]
nodes = to getNodes

connections :: Getter State [Connection]
connections = to getConnections

getNodes :: State -> [Node]
getNodes = HashMap.elems . getNodesMap

getNodesMap :: State -> NodesMap
getNodesMap = view nodesMap

getConnections :: State -> [Connection]
getConnections = HashMap.elems . getConnectionsMap

getConnectionsMap :: State -> ConnectionsMap
getConnectionsMap = view connectionsMap

getConnectionNodeIds :: ConnectionId -> State -> Maybe (NodeId, NodeId)
getConnectionNodeIds connId state = connectionToNodeIds <$> conn
    where conn = lookUpConnection state connId

updateNodes :: NodesMap -> State -> State
updateNodes newNodesMap state = state & nodesMap .~ newNodesMap

addNode :: Node -> State -> State
addNode newNode state  = state & nodesMap . at (newNode ^. Node.nodeId) ?~ newNode

removeNode :: NodeId -> State -> State
removeNode remNodeId state = state & nodesMap . at remNodeId .~ Nothing

-- TODO[react]: Consider ConnectionId as new type
addConnection :: OutPortRef -> InPortRef -> Command State ConnectionId
addConnection sourcePortRef destPortRef = do
    connectionsMap . at destPortRef ?= Connection sourcePortRef destPortRef
    return destPortRef

removeConnections :: [ConnectionId] -> State -> State
removeConnections connIds state = foldr removeConnection state connIds

removeConnection :: ConnectionId -> State -> State
removeConnection connId state = state & connectionsMap . at connId .~ Nothing

lookUpConnection :: State -> ConnectionId -> Maybe Connection
lookUpConnection state connId = HashMap.lookup connId $ getConnectionsMap state

containsNode :: NodeId -> Connection -> Bool
containsNode nid conn = startsWithNode nid conn
                    || endsWithNode   nid conn

startsWithNode :: NodeId -> Connection -> Bool
startsWithNode nid conn = conn ^. Connection.src . PortRef.srcNodeId == nid

endsWithNode :: NodeId -> Connection -> Bool
endsWithNode nid conn = conn ^. Connection.dst . PortRef.dstNodeId == nid

connectionsContainingNode :: NodeId -> State -> [Connection]
connectionsContainingNode nid state = filter (containsNode nid) $ getConnections state

connectionsContainingNodes :: [NodeId] -> State -> [Connection]
connectionsContainingNodes nodeIds state = let nodeIdsSet = Set.fromList nodeIds
    in flip filter (getConnections state) $ \conn -> (
        (Set.member (conn ^. Connection.src . PortRef.srcNodeId) nodeIdsSet) ||
        (Set.member (conn ^. Connection.dst . PortRef.dstNodeId) nodeIdsSet))

connectionsToNodes :: Set.Set NodeId -> State -> [Connection]
connectionsToNodes nodeIds state = filter ((flip Set.member nodeIds) . (view $ Connection.dst . PortRef.dstNodeId)) $ getConnections state

connectionIdsContainingNode :: NodeId -> State -> [ConnectionId]
connectionIdsContainingNode nid state = view Connection.connectionId <$> connectionsContainingNode nid state

connectionsToNodesIds :: Set.Set NodeId -> State -> [ConnectionId]
connectionsToNodesIds nodeIds state = view Connection.connectionId <$> connectionsToNodes nodeIds state

hasConnections :: NodeId -> State -> Bool
hasConnections = (not . null) .: connectionsContainingNode
