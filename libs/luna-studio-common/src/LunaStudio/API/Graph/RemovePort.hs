module LunaStudio.API.Graph.RemovePort where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Connection    (Connection)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.PortRef       (OutPortRef)
import           Prologue


data Request = Request
    { _location :: GraphLocation
    , _portRef  :: OutPortRef
    } deriving (Eq, Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request
instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.node.removePort"
