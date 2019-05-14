module Bus.Data.Environment where

import Prologue

import qualified System.ZMQ4 as Zmq


-- === Definition === --

data Environment = Environment
    { _pubSocket :: Zmq.Socket Zmq.Pub
    , _subSocket :: Zmq.Socket Zmq.Sub
    }
makeLenses ''Environment
