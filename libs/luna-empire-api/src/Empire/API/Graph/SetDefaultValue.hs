module Empire.API.Graph.SetDefaultValue where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.PortRef       (AnyPortRef)
import           Empire.API.Data.DefaultValue  (PortDefault)
import qualified Empire.API.Response            as Response
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T
import qualified Empire.API.Request            as R

data Request = Request { _location     :: GraphLocation
                       , _portRef      :: AnyPortRef
                       , _defaultValue :: PortDefault
                       } deriving (Generic, Show, Eq)


type Response = Response.SimpleResponse Request ()
instance Response.ResponseResult Request () ()

makeLenses ''Request
instance Binary Request
instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.defaultValue"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
