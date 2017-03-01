module Empire.API.Graph.MovePort where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node)
import           Empire.API.Data.PortRef       (AnyPortRef)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T



data Request = Request { _location    :: GraphLocation
                       , _anyPortRef  :: AnyPortRef
                       , _newPosition :: Int
                       } deriving (Generic, Eq, NFData, Show)

--TODO[MM]: Remove Node as Response Result. We don't use it
type Result = Node

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result

makeLenses ''Request
instance Binary Request
instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.movePort"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response