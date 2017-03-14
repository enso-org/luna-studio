module Empire.API.Graph.GetSubgraph where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.Graph         (Graph)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.MonadPath     (MonadPath)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T



data Request = Request { _location    :: GraphLocation
                       } deriving (Generic, Eq, NFData, Show)

data Result = Result { _graph  :: Graph
                     } deriving (Generic, Eq, NFData, Show)

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

makeLenses ''Request
makeLenses ''Result

instance Binary Request
instance Binary Result
instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.getSubgraph"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response
