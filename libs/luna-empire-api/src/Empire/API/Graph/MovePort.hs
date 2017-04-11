module Empire.API.Graph.MovePort where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (InputSidebar)
import           Empire.API.Data.PortRef       (OutPortRef)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T



data Request = Request { _location    :: GraphLocation
                       , _portRef     :: OutPortRef
                       , _newPortPos  :: Int
                       } deriving (Generic, Eq, NFData, Show)

type Result = InputSidebar

type Response = Response.Response Request () Result
instance Response.ResponseResult Request () Result


makeLenses ''Request
instance Binary Request
instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.movePort"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response             where topic _ = topicPrefix <> T.response
