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
           deriving (Eq, Generic, NFData, Show)

data Update = Update { _location  :: GraphLocation
                     , _clientId  :: ClientId
                     , _event     :: Event
                     } deriving (Eq, Generic, NFData, Show)

makeLenses ''Update
makeLenses ''Event
instance Binary Update
instance Binary Event


topicPrefix = "empire.graph.collaboration"
instance T.MessageTopic Update where topic _ = topicPrefix <> T.update
