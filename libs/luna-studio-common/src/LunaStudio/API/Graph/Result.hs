module LunaStudio.API.Graph.Result where

import           Data.Binary                (Binary)
import           LunaStudio.Data.Connection (ConnectionId)
import           LunaStudio.Data.Graph      (Graph)
import           LunaStudio.Data.Node       (NodeId)
import           Prologue


data Result = Result { _removedNodes       :: [NodeId]
                     , _removedConnections :: [ConnectionId]
                     , _graphUpdates       :: Graph
                     } deriving (Eq, Generic, NFData, Show)

makeLenses ''Result
instance Binary Result
