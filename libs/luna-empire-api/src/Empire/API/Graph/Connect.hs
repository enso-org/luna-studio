module Empire.API.Graph.Connect where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.PortRef       (OutPortRef(..), InPortRef(..))
import qualified Empire.API.Response             as Response
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T
import qualified Empire.API.Request            as R

data Request = Request { _location  :: GraphLocation
                       , _src       :: OutPortRef
                       , _dst       :: InPortRef
                       } deriving (Generic, Show, Eq)

data Inverse = Inverse { _locationPrev  :: GraphLocation
                       , _srcPrev       :: OutPortRef
                       , _dstPrev       :: InPortRef
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request Inverse
instance Response.ResponseResult Request Inverse ()

data Update  = Update  { _location'  :: GraphLocation
                       , _src'       :: OutPortRef
                       , _dst'       :: InPortRef
                       } deriving (Generic, Show, Eq)


makeLenses ''Request
makeLenses ''Update
makeLenses ''Inverse
instance Binary Request
instance Binary Update
instance Binary Inverse

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.connect"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
