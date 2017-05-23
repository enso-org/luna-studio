{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Data.Graph where

import           Common.Prelude
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.HashMap.Strict        (HashMap)
import           LunaStudio.Data.Connection (Connection, ConnectionId)
import           LunaStudio.Data.Node       (ExpressionNode)
import           LunaStudio.Data.NodeLoc    (NodeLoc)


type NodesMap       = HashMap NodeLoc ExpressionNode
type ConnectionsMap = HashMap ConnectionId Connection

data Graph = Graph { _nodesMap             :: NodesMap
                   , _connectionsMap       :: ConnectionsMap
                   } deriving (Default, Eq, FromJSON, Generic, Show, ToJSON)

makeLenses ''Graph
