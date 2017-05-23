module LunaStudio.API.Graph.GetSubgraphs where

import           Data.Binary                   (Binary)
import           Data.Map                      (Map)
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Request        as R
import qualified LunaStudio.API.Response       as Response
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.Breadcrumb    (BreadcrumbItem)
import           LunaStudio.Data.Graph         (Graph)
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           Prologue


data Request = Request { _location    :: GraphLocation
                       } deriving (Eq, Generic, NFData, Show)

data Result = Result { _graphs :: Map BreadcrumbItem Graph
                     } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request
makeLenses ''Result
instance Binary Request
instance Binary Result
instance G.GraphRequest Request where location = location


type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

topicPrefix :: T.Topic
topicPrefix = "empire.graph.node.getSubgraph"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response
