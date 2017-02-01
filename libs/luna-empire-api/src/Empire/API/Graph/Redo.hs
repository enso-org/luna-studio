module Empire.API.Graph.Redo where

import Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T
import qualified Empire.API.Response           as Response
import qualified Empire.API.Request            as R

data RedoRequest = RedoRequest deriving (Generic, Show, Eq)

data Request = Request { _request :: RedoRequest
                       } deriving (Generic, Show, Eq)

makeLenses ''RedoRequest
makeLenses ''Request

instance Binary RedoRequest
instance Binary Request

type Response = Response.Response Request () ()
instance Response.ResponseResult Request () ()


topicPrefix = "empire.redo"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
