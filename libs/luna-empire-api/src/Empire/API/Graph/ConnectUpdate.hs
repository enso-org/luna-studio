module Empire.API.Graph.ConnectUpdate where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.Connection    (Connection)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.PortRef       (InPortRef (..), OutPortRef (..))
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T


data Update  = Update  { _location'   :: GraphLocation
                       , _connection' :: Connection
                       } deriving (Generic, Eq, NFData, Show)

makeLenses ''Update
instance Binary Update

topicPrefix = "empire.graph.connect"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
