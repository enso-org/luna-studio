module LunaStudio.API.Graph.Redo where

import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           Prologue


data RedoRequest = RedoRequest deriving (Eq, Generic, NFData, Show)

data Request = Request { _request :: RedoRequest
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''RedoRequest
makeLenses ''Request
instance Binary RedoRequest
instance Binary Request


type Response = Response.Response Request () ()
instance Response.ResponseResult Request () ()

topicPrefix :: T.Topic
topicPrefix = "empire.redo"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
