{-# LANGUAGE ConstraintKinds #-}

module ZMQ.RPC.Client where

import           Control.Error        (ExceptT, hoistEither)
import qualified Data.Binary          as Binary
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           System.ZMQ4.Monadic  (ZMQ)
import qualified System.ZMQ4.Monadic  as ZMQ

import           Prologue             hiding (hoistEither)
import           ZMQ.RPC.Handler      (Serializable)
import           ZMQ.RPC.Response     (Response)
import qualified ZMQ.RPC.Response     as Response



type Error = String


query :: (ZMQ.Sender t, ZMQ.Receiver t, Serializable request result)
      => ZMQ.Socket z t
      -> request
      -> ExceptT Error (ZMQ z) result
query socket request = do
    response <- queryRaw socket request
    hoistEither $ processResponse response


queryRaw :: (ZMQ.Sender t, ZMQ.Receiver t, Serializable request result)
          => ZMQ.Socket z t -> request -> ExceptT Error (ZMQ z) (Response result)
queryRaw socket request = do
    lift $ ZMQ.send socket [] $ toStrict $ Binary.encode request
    encoded_response <- lift $ ZMQ.receive socket
    hoistEither $ case Binary.decodeOrFail $ fromStrict encoded_response of
        Right r -> Right $ r ^. _3
        Left  l -> Left $ l ^. _3


processResponse :: Response result -> Either Error result
processResponse (Response.Result _ result) = return result
processResponse (Response.Exception _ msg) = Left msg
