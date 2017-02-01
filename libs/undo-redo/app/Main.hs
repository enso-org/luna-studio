{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Prologue

import qualified Undo                 as Undo

import qualified ZMQ.Bus.EndPoint     as EP
import qualified ZMQ.Bus.Config       as Config

main :: IO ()
main = do
    endPoints <- EP.clientFromConfig <$> Config.load
    r <- Undo.run endPoints
    return ()
