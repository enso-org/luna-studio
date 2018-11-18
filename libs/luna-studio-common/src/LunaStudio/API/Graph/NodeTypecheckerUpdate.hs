module LunaStudio.API.Graph.NodeTypecheckerUpdate where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Node          (NodeTypecheckerUpdate)
import           Prologue


data Update = Update
    { _location  :: GraphLocation
    , _node      :: NodeTypecheckerUpdate
    } deriving (Eq, Generic, Show)

makeLenses ''Update

instance Binary Update
instance NFData Update
instance ToJSON Update


topicPrefix :: T.Topic
topicPrefix = "empire.graph.node"
instance T.MessageTopic Update where topic = topicPrefix <> T.typecheck
