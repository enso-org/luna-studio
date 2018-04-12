module LunaStudio.API.Response where

import           Data.Aeson.Types       (ToJSON)
import           Data.Binary            (Binary)
import           Data.UUID.Types        (UUID)
import           LunaStudio.API.Request (Request (..))
import           LunaStudio.API.Topic   (MessageTopic)
import           LunaStudio.Data.Error  (Error, LunaError)
import           Prologue


data Status a 
    = Ok    { _resultData :: a }
    | Error { _lunaError  :: Error LunaError }
    deriving (Eq, Generic, Show)

makeLenses ''Status
makePrisms ''Status

instance Binary a => Binary (Status a)
instance NFData a => NFData (Status a)
instance ToJSON a => ToJSON (Status a)

data Response req inv res = Response
    { _requestId :: UUID
    , _guiID     :: Maybe UUID
    , _request   :: req
    , _inverse   :: Status inv
    , _status    :: Status res
    } deriving (Eq, Generic, Show)

type SimpleResponse req inv = Response req inv ()


class
    ( MessageTopic (Request req)
    , MessageTopic (Response req inv res)
    , Binary req
    , Binary inv
    , Binary res
    ) => ResponseResult req inv res | req -> inv res where
      
        result :: Request req -> inv -> res -> Response req inv res
        result (Request uuid guiID req) inv payload
            = Response uuid guiID req (Ok inv) (Ok payload)

        error :: Request req -> Status inv -> Error LunaError
            -> Response req inv res
        error  (Request uuid guiID req) inv err
            = Response uuid guiID req inv (Error err)

ok :: (ResponseResult req inv (), MessageTopic (Response req inv ())) 
   => Request req -> inv -> Response req inv ()
ok (Request uuid guiID req) inv = Response uuid guiID req (Ok inv) (Ok ())

makeLenses ''Response

instance (Binary req, Binary res, Binary inv) => Binary (Response req inv res)
instance (NFData req, NFData res, NFData inv) => NFData (Response req inv res)
instance (ToJSON req, ToJSON res, ToJSON inv) => ToJSON (Response req inv res)
