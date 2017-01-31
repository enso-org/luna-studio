{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module ZMQ.RPC.Server.Processor where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Binary            (Binary)
import qualified Data.Binary            as Binary
import           Data.ByteString        (ByteString)
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           System.ZMQ4.Monadic    (ZMQ)

import           Prologue               hiding (error, trace)
import           System.Log.MLogger
import           ZMQ.RPC.Handler        (RPCHandler)
import           ZMQ.RPC.Response       (Response)
import qualified ZMQ.RPC.Response       as Response
import qualified ZMQ.RPC.RPC            as RPC


logger :: Logger
logger = getLogger $moduleName


process :: (Binary request, Binary result, Show result) => RPCHandler request result
                     -> ByteString -> Int -> ZMQ z ByteString
process handler encodedRequest requestID = toStrict . Binary.encode <$> case Binary.decodeOrFail $ fromStrict encodedRequest of
    Left  err     -> responseError requestID $ err ^. _3
    Right request -> handler call $ request ^. _3 where
        call method = do
            result <- RPC.run $ method $ request ^. _3
            either (responseError requestID) (responseResult requestID) result


responseResult :: (Show result, MonadIO m) => Int -> result -> m (Response result)
responseResult requestID result = do
    logger trace $ show result
    return $ Response.Result requestID result


responseError :: Int -> RPC.Error -> ZMQ z (Response result)
responseError requestID err = do
    logger error err
    return $ Response.Exception requestID err
