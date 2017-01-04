{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Empire.API.Response where

import           Prologue

import           Data.Binary (Binary)
import           Data.UUID.Types (UUID)
import           Empire.API.Topic (MessageTopic)
import           Empire.API.Request (Request(..))

data Status a = Ok    { _resultData  :: a }
              | Error { _message     :: String }
              deriving (Generic, Show, Eq)

instance (Binary a) => Binary (Status a)
makeLenses ''Status
makePrisms ''Status

data Response req inv res = Response { _requestId :: UUID
                                     , _guiID  :: Maybe UUID
                                     , _request   :: req
                                     , _inverse   :: Status inv
                                     , _status    :: Status res
                                     }
                      deriving (Generic, Show, Eq)

type SimpleResponse req inv = Response req inv ()

class (MessageTopic (Request req), MessageTopic (Response req inv res), Binary req, Binary inv, Binary res) => ResponseResult req inv res | req -> inv res where
  result :: Request req -> inv -> res -> Response req inv res
  result (Request uuid guiID req) inv payload = Response uuid guiID req (Ok inv) (Ok payload)

  error :: Request req -> String-> Response req inv res
  error  (Request uuid guiID req) msg = Response uuid guiID req (Error msg) (Error msg)

ok :: (ResponseResult req inv (), MessageTopic (Response req inv ())) => Request req -> inv -> Response req inv ()
ok (Request uuid guiID req) inv = Response uuid guiID req (Ok inv) (Ok ())

makeLenses ''Response

instance (Binary req, Binary res, Binary inv) => Binary (Response req inv res)
