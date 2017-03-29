module Empire.API.Graph.CollaborationUpdate where

import           Data.Binary                   (Binary)
import           Prologue

import           Data.UUID.Types               (UUID)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.NodeLoc       (NodeLoc)
import qualified Empire.API.Topic              as T

type ClientId = UUID

data Event = Modify      [NodeLoc]
           | Touch       [NodeLoc]
           | CancelTouch [NodeLoc]
           | Refresh
           deriving (Generic, Eq, NFData, Show)

data Update = Update { _location  :: GraphLocation
                     , _clientId  :: ClientId
                     , _event     :: Event
                     } deriving (Generic, Eq, NFData, Show)


makeLenses ''Update
makeLenses ''Event

instance Binary Update
instance Binary Event

topicPrefix = "empire.graph.collaboration"
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
