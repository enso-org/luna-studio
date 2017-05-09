module Empire.API.Graph.AddConnection where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.Connection    (Connection)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node, NodeId)
import           Empire.API.Data.NodeLoc       (NodeLoc)
import           Empire.API.Data.PortRef       (AnyPortRef, OutPortRef)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T


data Request = Request { _location :: GraphLocation
                       , _src      :: Either OutPortRef NodeId
                       , _dst      :: Either AnyPortRef NodeLoc
                       } deriving (Generic, Eq, NFData, Show)

data Result = Result { _connection :: Connection
                     , _srcNode    :: Node
                     , _dstNode    :: Node
                     } deriving (Generic, Eq, NFData, Show)

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

makeLenses ''Request
makeLenses ''Result
instance Binary Request
instance Binary Result

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.connect"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
