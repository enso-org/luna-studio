module LunaStudio.API.Graph.CollaborationUpdate where

import           Data.Binary                   (Binary)
import           Data.UUID.Types               (UUID)
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.NodeLoc       (NodeLoc)
import           Prologue


type ClientId = UUID

data Event = Modify      [NodeLoc]
           | Touch       [NodeLoc]
           | CancelTouch [NodeLoc]
           | Refresh
           deriving (Eq, Generic, Show)

data Update = Update { _location  :: GraphLocation
                     , _clientId  :: ClientId
                     , _event     :: Event
                     } deriving (Eq, Generic, Show)

makeLenses ''Update
makeLenses ''Event
instance Binary Update
instance NFData Update
instance Binary Event
instance NFData Event


topicPrefix :: T.Topic
topicPrefix = "empire.graph.collaboration"
instance T.MessageTopic Update where topic _ = topicPrefix <> T.update
