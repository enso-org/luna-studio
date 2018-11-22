module LunaStudio.API.Graph.CollapseToFunction where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.NodeCache     (NodeCache)
import           LunaStudio.Data.NodeLoc       (NodeLoc)
import           Prologue                      hiding (TypeRep)


data Request = Request
    { _location :: GraphLocation
    , _nodeLocs :: [NodeLoc]
    } deriving (Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance ToJSON Request
instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.node.collapsetofunction"
