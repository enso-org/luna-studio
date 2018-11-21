module LunaStudio.API.Graph.AddConnection where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Connection    (ConnectionId)
import           LunaStudio.Data.Diff          (Diff)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.NodeId        (NodeId)
import           LunaStudio.Data.NodeLoc       (NodeLoc)
import           LunaStudio.Data.PortRef       (AnyPortRef, OutPortRef)
import           Prologue


data Request = Request
    { _location :: GraphLocation
    , _src      :: Either OutPortRef NodeId
    , _dst      :: Either AnyPortRef NodeLoc
    } deriving (Eq, Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request

instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.connect"
