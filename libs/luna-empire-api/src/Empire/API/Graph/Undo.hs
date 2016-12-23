module Empire.API.Graph.Undo where

import Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T
import qualified Empire.API.Response           as Response
import qualified Empire.API.Request            as R

data Action  = UndoRequest | RedoRequest deriving (Generic, Show, Eq)

data Request = Request {_request  :: Action
                       } deriving (Generic, Show, Eq)

makeLenses ''Action
makeLenses ''Request

instance Binary Action
instance Binary Request
 
type Response = Response.Response Request () ()
instance Response.ResponseResult Request () ()


topicPrefix = "empire.undo"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
