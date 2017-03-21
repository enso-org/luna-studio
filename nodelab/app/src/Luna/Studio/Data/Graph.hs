{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Data.Graph where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.HashMap.Strict        (HashMap)
import           Empire.API.Data.Connection (Connection, ConnectionId)
import           Empire.API.Data.Node       (Node, NodeId)
import           Luna.Studio.Prelude


type NodesMap       = HashMap NodeId Node
type ConnectionsMap = HashMap ConnectionId Connection

data Graph = Graph { _nodesMap             :: NodesMap
                   , _connectionsMap       :: ConnectionsMap
                   } deriving (Default, Eq, FromJSON, Generic, Show, ToJSON)

makeLenses ''Graph
