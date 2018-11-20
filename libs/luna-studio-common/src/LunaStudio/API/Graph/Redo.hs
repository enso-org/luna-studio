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
type instance Response.InverseOf Request = ()
type instance Response.ResultOf  Request = ()

instance T.MessageTopic Request where
    topic = "empire.redo"
