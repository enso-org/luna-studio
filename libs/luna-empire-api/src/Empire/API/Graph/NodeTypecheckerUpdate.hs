module Empire.API.Graph.NodeTypecheckerUpdate where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeTypecheckerUpdate)
import qualified Empire.API.Topic              as T

data Update = Update { _location  :: GraphLocation
                     , _node      :: NodeTypecheckerUpdate
                     } deriving (Generic, Eq, NFData, Show)

makeLenses ''Update

instance Binary Update

topicPrefix = "empire.graph.node"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.typecheck
