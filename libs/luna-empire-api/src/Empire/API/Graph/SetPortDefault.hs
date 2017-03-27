module Empire.API.Graph.SetPortDefault where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.PortDefault   (PortDefault)
import           Empire.API.Data.PortRef       (AnyPortRef)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T

data Request = Request { _location     :: GraphLocation
                       , _portRef      :: AnyPortRef
                       , _defaultValue :: PortDefault
                       } deriving (Generic, Eq, NFData, Show)

data Inverse = Inverse { _prevPortDefault :: PortDefault
                       } deriving (Generic, Eq, NFData, Show)


type Response = Response.SimpleResponse Request Inverse
instance Response.ResponseResult Request Inverse ()

makeLenses ''Request
instance Binary Request
instance G.GraphRequest Request where location = location

makeLenses ''Inverse
instance Binary Inverse

topicPrefix = "empire.graph.node.defaultValue"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response