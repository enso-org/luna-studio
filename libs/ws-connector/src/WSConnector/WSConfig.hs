{-# LANGUAGE TemplateHaskell #-}

module WSConnector.WSConfig where

import           Prologue
import qualified ZMQ.Bus.WS.Config as WSConfig

data Config = Config
    { _host        :: String
    , _fromWebPort :: Int
    , _toWebPort   :: Int
    , _pingTime    :: Int
    } deriving (Read, Show, Eq)

makeLenses ''Config

readWebsocketConfig config = Config host fromWebPort toWebPort pingTime where
    host        = WSConfig.host websocket
    fromWebPort = unsafeRead (WSConfig.fromWebPort websocket)
    toWebPort   = unsafeRead (WSConfig.toWebPort   websocket)
    pingTime    = unsafeRead (WSConfig.pingTime    websocket)
    websocket   = WSConfig.websocket config
