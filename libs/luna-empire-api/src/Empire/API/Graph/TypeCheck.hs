module Empire.API.Graph.TypeCheck where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T
import qualified Empire.API.Response           as Response
import qualified Empire.API.Request            as R

data Request = Request { _location :: GraphLocation
                       } deriving (Generic, Show, Eq)

makeLenses ''Request

instance Binary Request
instance G.GraphRequest Request where location = location

type Response = Response.Response Request ()
instance Response.ResponseResult Request ()


topicPrefix = "empire.environment.debug.typecheck"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
