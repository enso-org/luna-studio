module LunaStudio.API.Graph.Redo where

import           Data.Aeson.Types        (ToJSON)
import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           Prologue


data RedoRequest = RedoRequest deriving (Eq, Generic, Show)

data Request     = Request
    { _request :: RedoRequest
    } deriving (Eq, Generic, Show)

makeLenses ''RedoRequest
makeLenses ''Request

instance Binary RedoRequest
instance NFData RedoRequest
instance ToJSON RedoRequest
instance Binary Request
instance NFData Request
instance ToJSON Request


type Response = Response.Response Request () ()
instance Response.ResponseResult Request () ()

topicPrefix :: T.Topic
topicPrefix = "empire.redo"
instance T.MessageTopic (R.Request Request) where
    topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where
    topic _ = topicPrefix <> T.response
