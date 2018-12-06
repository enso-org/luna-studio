module LunaStudio.API.Graph.SetCode where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.NodeCache     (NodeCache)
import           LunaStudio.Data.Point         (Point)
import           Prologue


data Request = Request
    { _location :: GraphLocation
    , _code     :: Text
    , _cache    :: NodeCache
    , _cursor   :: Maybe Point
    } deriving (Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request
instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.node.setCode"
