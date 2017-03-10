{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.State.Graph where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.HashMap.Strict        (HashMap)
import           Empire.API.Data.Connection (Connection, ConnectionId)
import           Empire.API.Data.Node       (Node, NodeId)
import           Luna.Studio.Prelude


type NodesMap       = HashMap NodeId Node
type ConnectionsMap = HashMap ConnectionId Connection

data Graph = Graph { _nodesMap             :: NodesMap
                   , _connectionsMap       :: ConnectionsMap
                   } deriving (Show, Eq, Generic)

data Subgraph = Subgraph { _nodesList       :: [Node]
                         , _connectionsList :: [Connection]
                         } deriving (Show, Eq, Generic)

makeLenses ''Graph
makeLenses ''Subgraph

instance ToJSON Graph
instance ToJSON Subgraph
instance FromJSON Subgraph
instance Default Graph where
    def = Graph def def
instance Default Subgraph where
    def = Subgraph def def
