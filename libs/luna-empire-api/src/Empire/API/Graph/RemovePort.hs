module Empire.API.Graph.RemovePort where

import           Data.Binary                   (Binary)
import           Prologue

import           Data.Map                      (Map (..))
import           Empire.API.Data.Connection    (Connection)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (InputSidebar)
import           Empire.API.Data.PortRef       (OutPortRef)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T


data Request = Request { _location :: GraphLocation
                       , _portRef  :: OutPortRef
                       } deriving (Generic, Eq, NFData, Show)

data Inverse = Inverse { _connections :: [Connection]
                       } deriving (Generic, Eq, NFData, Show)

type Result = InputSidebar

type Response = Response.Response Request Inverse Result
instance Response.ResponseResult Request Inverse Result

makeLenses ''Request
makeLenses ''Inverse
instance Binary Request
instance Binary Inverse

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.removePort"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response
