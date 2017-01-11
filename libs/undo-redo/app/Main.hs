{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Prologue

import           Cmd         (Cmd)
import qualified Cmd         as Cmd
import           Version

import qualified System.Log.Options   as Opt
import           System.Log.Options (help, long, metavar, short)
import qualified Undo                 as Undo

import           System.Log.MLogger
import qualified ZMQ.Bus.EndPoint     as EP
import qualified ZMQ.Bus.Config       as Config

import           Control.Monad (forever)

import System.IO (stdout,hFlush)


main :: IO ()
main = do
    endPoints <- EP.clientFromConfig <$> Config.load
    r <- Undo.run endPoints
    case r of
        Left err -> return ()
        Right _  -> return ()
