{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module ZMQ.RPC.Server.Server where

import           System.ZMQ4.Monadic      (ZMQ)
import qualified System.ZMQ4.Monadic      as ZMQ

import           Prologue
import           System.Log.MLogger
import           ZMQ.RPC.Handler          (RPCHandler, Serializable)
import qualified ZMQ.RPC.Server.Processor as Processor



logger :: Logger
logger = getLogger $moduleName


run :: Serializable request result
    => Int -> String -> RPCHandler request result -> IO ()
run workerCount endpoint handler = ZMQ.runZMQ $ serve workerCount endpoint handler


handleCalls :: (ZMQ.Receiver t, ZMQ.Sender t, Serializable request result)
            => ZMQ.Socket z t -> RPCHandler request result -> ZMQ z ()
handleCalls socket handler = forM_ [1..] $ handleCall socket handler


handleCall :: (ZMQ.Receiver t, ZMQ.Sender t, Serializable request result)
           => ZMQ.Socket z t -> RPCHandler request result -> Int -> ZMQ z ()
handleCall socket handler requestID = do
    encoded_request  <- ZMQ.receive socket
    encoded_response <- Processor.process handler encoded_request requestID
    ZMQ.send socket [] encoded_response


serve :: Serializable request result
      => Int -> String -> RPCHandler request result -> ZMQ z ()
serve workerCount endpoint  handler = do
    router <- ZMQ.socket ZMQ.Router
    dealer <- ZMQ.socket ZMQ.Dealer
    let internalEndpoint = "inproc://rpcworker"
    ZMQ.bind router endpoint
    ZMQ.bind dealer internalEndpoint
    forM_ [0..workerCount] $ \_ -> ZMQ.async $ do
        rep <- ZMQ.socket ZMQ.Rep
        ZMQ.connect rep internalEndpoint
        handleCalls rep handler
    ZMQ.proxy router dealer Nothing
