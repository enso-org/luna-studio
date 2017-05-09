module Empire.API.Graph.Result where

import           Data.Binary                (Binary)
import           Empire.API.Data.Connection (ConnectionId)
import           Empire.API.Data.Graph      (Graph)
import           Empire.API.Data.Node       (NodeId)
import           Prologue


data Result = Result { _removedNodes       :: [NodeId]
                     , _removedConnections :: [ConnectionId]
                     , _graphUpdates       :: Graph
                     } deriving (Generic, Eq, NFData, Show)

makeLenses ''Result
instance Binary Result
