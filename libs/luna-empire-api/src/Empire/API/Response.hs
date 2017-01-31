{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Empire.API.Response where

import           Prologue

import           Data.Binary        (Binary)
import           Data.UUID.Types    (UUID)
import           Empire.API.Request (Request (..))
import           Empire.API.Topic   (MessageTopic)

data Status a = Ok    { _resultData  :: a }
              | Error { _message     :: String }
              deriving (Eq, Generic, NFData, Show)

instance (Binary a) => Binary (Status a)
makeLenses ''Status
makePrisms ''Status

data Response req res = Response { _requestId :: UUID
                                 , _request  :: req
                                 , _status   :: Status res
                                 }
                      deriving (Eq, Generic, NFData, Show)

type SimpleResponse req = Response req ()

class (MessageTopic (Request req), MessageTopic (Response req res), Binary req, Binary res) => ResponseResult req res | req -> res where
  result :: Request req -> res -> Response req res
  result (Request uuid req) payload = Response uuid req (Ok payload)

  error :: Request req -> String -> Response req res
  error  (Request uuid req) msg     = Response uuid req (Error msg)

ok :: (ResponseResult req (), MessageTopic (Response req ())) => Request req -> Response req ()
ok (Request uuid req) = Response uuid req (Ok ())

makeLenses ''Response

instance (Binary req, Binary res) => Binary (Response req res)
