module Main where

import           Prologue

import qualified Undo

import           GHC.IO.Encoding      (setLocaleEncoding, utf8)
import qualified ZMQ.Bus.EndPoint     as EP
import qualified ZMQ.Bus.Config       as Config

main :: IO ()
main = do
    setLocaleEncoding utf8
    endPoints <- EP.clientFromConfig <$> Config.load
    r <- Undo.run endPoints
    return ()
