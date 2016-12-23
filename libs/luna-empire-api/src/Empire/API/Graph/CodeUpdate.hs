module Empire.API.Graph.CodeUpdate where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node)
import qualified Empire.API.Topic              as T

data Update = Update { _location :: GraphLocation
                     , _code     :: Text
                     } deriving (Generic, Show, Eq)

makeLenses ''Update

instance Binary Update

topicPrefix = "empire.graph.code"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
