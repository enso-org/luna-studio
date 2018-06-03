{-# LANGUAGE TemplateHaskell #-}

module WSConnector.WSConfig where

import           Prologue
import qualified ZMQ.Bus.WS.Config as FD

data Config = Config { _host        :: String
                     , _fromWebPort :: Int
                     , _toWebPort   :: Int
                     , _pingTime    :: Int
                     } deriving (Read, Show, Eq)

makeLenses ''Config

readWebsocketConfig config = Config host fromWebPort toWebPort pingTime where
    host        = FD.host websocket
    fromWebPort = unsafeRead (FD.fromWebPort websocket)
    toWebPort   = unsafeRead (FD.toWebPort   websocket)
    pingTime    = unsafeRead (FD.pingTime    websocket)
    websocket   = FD.websocket config
