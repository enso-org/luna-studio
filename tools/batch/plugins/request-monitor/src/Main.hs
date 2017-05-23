{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           GHC.IO.Encoding      (setLocaleEncoding, utf8)
import           Prologue              hiding (argument)
import           System.Console.Docopt
import           System.Environment    (getArgs)

import qualified Empire.Monitor        as Monitor
import           System.Log.MLogger
import qualified ZMQ.Bus.Config        as Config
import qualified ZMQ.Bus.EndPoint      as EP

patterns :: Docopt
patterns = [docoptFile|src/RequestMonitorUsage.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

rootLogger :: Logger
rootLogger = getLogger ""

logger :: Logger
logger = getLogger $moduleName

main :: IO ()
main = do
    setLocaleEncoding utf8
    args <- parseArgsOrExit patterns =<< getArgs
    endPoints <- EP.clientFromConfig <$> Config.load
    projectRoot <- Config.projectRoot <$> Config.projects <$> Config.load
    print projectRoot
    when (args `isPresent` command "runOnIdle") $ do
        time       <- args `getArgOrExit` argument "seconds"
        script     <- args `getArgOrExit` argument "script"
        runOnIdle endPoints projectRoot (read time) script

runOnIdle :: EP.BusEndPoints -> FilePath -> Integer -> FilePath -> IO ()
runOnIdle endPoints projectRoot time script = do
    rootLogger setIntLevel 3
    Monitor.run endPoints projectRoot time script >>= \case
        Left err -> logger criticalFail err
        _        -> return ()
