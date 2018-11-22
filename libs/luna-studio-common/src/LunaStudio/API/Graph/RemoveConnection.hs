module LunaStudio.API.Graph.RemoveConnection where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Connection    (ConnectionId)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           Prologue


data Request = Request
    { _location :: GraphLocation
    , _connId   :: ConnectionId
    } deriving (Eq, Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request
instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.disconnect"
