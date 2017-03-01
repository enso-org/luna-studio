module Empire.API.Graph.Connect where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.Connection    (Connection)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.PortRef       (InPortRef (..), OutPortRef (..))
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T


data Request = Request { _location :: GraphLocation
                       , _src      :: Either OutPortRef NodeId
                       , _dst      :: Either InPortRef  NodeId
                       } deriving (Generic, Eq, NFData, Show)


data Update  = Update  { _location'   :: GraphLocation
                       , _connection' :: Connection
                       } deriving (Generic, Eq, NFData, Show)

type Result = Connection

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

makeLenses ''Request
makeLenses ''Update
instance Binary Request
instance Binary Update

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.connect"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
