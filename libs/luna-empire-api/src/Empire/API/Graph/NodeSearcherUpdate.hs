module Empire.API.Graph.NodeSearcherUpdate where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.NodeSearcher  (Items)
import qualified Empire.API.Topic              as T

data Update = Update { _location         :: GraphLocation
                     , _nodeSearcherData :: Items
                     } deriving (Generic, Eq, NFData, Show)

makeLenses ''Update

instance Binary Update

topicPrefix = "empire.graph.nodeSearcher"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
