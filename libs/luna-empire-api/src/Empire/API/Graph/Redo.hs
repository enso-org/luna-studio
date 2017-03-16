module Empire.API.Graph.Redo where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Request            as R
import qualified Empire.API.Response           as Response
import qualified Empire.API.Topic              as T

data RedoRequest = RedoRequest deriving (Generic, Show, Eq, NFData)

data Request = Request { _request :: RedoRequest
                       } deriving (Generic, Show, Eq, NFData)

makeLenses ''RedoRequest
makeLenses ''Request

instance Binary RedoRequest
instance Binary Request

type Response = Response.Response Request () ()
instance Response.ResponseResult Request () ()


topicPrefix = "empire.redo"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
