module LunaStudio.API.Graph.NodeResultUpdate where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.NodeId        (NodeId)
import           LunaStudio.Data.NodeValue     (NodeValue)
import           Prologue


data Update = Update
    { _location :: GraphLocation
    , _nodeId   :: NodeId
    , _value    :: NodeValue
    , _execTime :: Integer
    } deriving (Eq, Generic, Show)

makeLenses ''Update

instance Binary Update
instance NFData Update
instance ToJSON Update


topicPrefix :: T.Topic
topicPrefix = "empire.graph.result"
instance T.MessageTopic Update where topic = topicPrefix <> T.update
