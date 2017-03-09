module Empire.API.Graph.RemoveConnection where

import           Data.Binary                   (Binary)
import           Empire.API.Data.Connection    (ConnectionId)
import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.PortRef       (OutPortRef)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T
import           Prologue


data Request = Request { _location :: GraphLocation
                       , _connId   :: ConnectionId
                       } deriving (Generic, Eq, NFData, Show)

data Inverse = Inverse { _src :: OutPortRef
                       } deriving (Generic, Show, Eq, NFData)

type Response = Response.SimpleResponse Request Inverse
instance Response.ResponseResult Request Inverse ()

data Update = Update   { _location' :: GraphLocation
                       , _connId'   :: ConnectionId
                       } deriving (Generic, Eq, NFData, Show)


makeLenses ''Request
makeLenses ''Update
makeLenses ''Inverse
instance Binary Request
instance Binary Update
instance Binary Inverse

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.disconnect"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
