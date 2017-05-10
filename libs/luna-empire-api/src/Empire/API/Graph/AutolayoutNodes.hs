module Empire.API.Graph.AutolayoutNodes where

import           Data.Binary                   (Binary)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.NodeLoc       (NodeLoc)
import           Empire.API.Data.Position      (Position)
import qualified Empire.API.Graph.Request      as G
import           Empire.API.Graph.Result       (Result)
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T
import           Prologue


data Request = Request { _location :: GraphLocation
                       , _nodeLocs :: [NodeLoc]
                       } deriving (Generic, Eq, NFData, Show)

data Inverse = Inverse { _prevPositions :: [(NodeLoc, Position)]
                       } deriving (Generic, Eq, NFData, Show)

type Response = Response.Response Request Inverse Result
instance Response.ResponseResult Request Inverse Result

makeLenses ''Request
makeLenses ''Inverse

instance Binary Request
instance Binary Inverse
instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.autolayoutNodes"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response
