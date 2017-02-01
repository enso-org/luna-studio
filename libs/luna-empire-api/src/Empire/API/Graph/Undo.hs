module Empire.API.Graph.Undo where

import Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T
import qualified Empire.API.Response           as Response
import qualified Empire.API.Request            as R

data UndoRequest  = UndoRequest deriving (Generic, Show, Eq, NFData)

data Request = Request {_request  :: UndoRequest
                       } deriving (Generic, Show, Eq, NFData)

makeLenses ''UndoRequest
makeLenses ''Request

instance Binary UndoRequest
instance Binary Request

type Response = Response.Response Request () ()
instance Response.ResponseResult Request () ()


topicPrefix = "empire.undo"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
