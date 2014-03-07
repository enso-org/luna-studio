---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}

module Flowbox.ZMQ.RPC.Client where

import           System.ZMQ4.Monadic             (ZMQ)
import qualified System.ZMQ4.Monadic             as ZMQ
import           Control.Monad.Trans.Either
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers                   as Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Rpc.Exception                  as Exception
import           Generated.Proto.Rpc.Response                   (Response)
import qualified Generated.Proto.Rpc.Response                   as Response
import qualified Generated.Proto.Rpc.Response.Type              as Type
import Control.Monad.Trans.Class (lift)


type Error = String


query :: (ZMQ.Sender t, ZMQ.Receiver t, Proto.Serializable request, Proto.Serializable result)
      => ZMQ.Socket z t
      -> request
      -> Extensions.Key Maybe Response result
      -> EitherT String (ZMQ z) result
query socket request rspKey = do
    response <- query_raw socket request
    hoistEither $ processResponse rspKey response


query_raw :: (ZMQ.Sender t, ZMQ.Receiver t, Proto.Serializable request)
          => ZMQ.Socket z t -> request -> EitherT String (ZMQ z) Response
query_raw socket request = do
    lift $ ZMQ.send socket [] $ Proto.messagePut' request
    encoded_response <- lift $ ZMQ.receive socket
    hoistEither $ Proto.messageGet' encoded_response


processResponse :: Proto.Serializable result
                => Extensions.Key Maybe Response result -> Response -> Either Error result
processResponse rspKey response  = case Response.type' response of
    Type.Result    -> Proto.getExt' rspKey response
    Type.Exception -> do exc <- Proto.getExt' Exception.rsp response
                         excMsg <- Exception.message exc <?> "Exception without message"
                         Left $ decodeP excMsg
