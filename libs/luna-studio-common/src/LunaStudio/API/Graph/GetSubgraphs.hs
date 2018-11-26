module LunaStudio.API.Graph.GetSubgraphs where

import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import           Data.Map                      (Map)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Breadcrumb    (BreadcrumbItem)
import           LunaStudio.Data.Graph         (Graph)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           Prologue


data Request = Request
    { _location :: GraphLocation
    } deriving (Eq, Generic, Show)

data Result  = Result
    { _graphs   :: Map BreadcrumbItem Graph
    } deriving (Eq, Generic, Show)

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance NFData Request
instance ToJSON Request
instance Binary Result
instance NFData Result
instance ToJSON Result

instance G.GraphRequest Request where location = location

instance T.MessageTopic Request  where
    topic = "empire.graph.node.getSubgraph"
