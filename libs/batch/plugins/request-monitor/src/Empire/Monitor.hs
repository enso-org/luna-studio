{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Empire.Monitor where

import           Control.Lens                     ((.=), to, use)
import           Control.Monad                    (forever, when)
import           Control.Monad.State              (StateT, evalStateT)
import qualified Data.Binary                      as Bin
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (unpack)
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Prologue                         hiding (when)
import           System.Cmd                       (system)
import           System.Directory                 (getCurrentDirectory)
import           System.Exit                      (exitFailure)

import qualified LunaStudio.API.Control.EmpireStarted as EmpireStarted
import           LunaStudio.API.Request               (Request)
import qualified LunaStudio.API.Topic                 as Topic
import qualified Empire.Commands.Library          as Library
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (MonitorEnv)
import qualified Empire.Env                       as Env
import qualified Empire.Utils                     as Utils

import qualified System.Log.MLogger               as Logger
import qualified ZMQ.Bus.Bus                      as Bus
import qualified ZMQ.Bus.Data.Message             as Message
import           ZMQ.Bus.Data.MessageFrame        (MessageFrame (MessageFrame))
import           ZMQ.Bus.Data.Topic               (Topic)
import           ZMQ.Bus.EndPoint                 (BusEndPoints)
import           ZMQ.Bus.Trans                    (BusT (..))
import qualified ZMQ.Bus.Trans                    as Bus

import           Data.Time.Clock.POSIX

defaultTopics :: [String]
defaultTopics = ["empire."]

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

run :: BusEndPoints -> FilePath -> Integer -> FilePath -> IO (Either Bus.Error ())
run endPoints projectRoot timeout script = do
    logger Logger.info $ "Script " <> script <> " scheduled to run after " <> show timeout <> " seconds of requests inactivity."
    dir <- getCurrentDirectory
    logger Logger.info $ "Working directory: " <> dir
    Bus.runBus endPoints $ do
        let formatted = True
        logger Logger.info $ "Subscribing to topics: " <> show defaultTopics
        logger Logger.info $ (Utils.display formatted) endPoints
        mapM_ Bus.subscribe defaultTopics
        Bus.runBusT $ evalStateT (runBus timeout script) def

runBus :: Integer -> FilePath -> StateT MonitorEnv BusT ()
runBus timeout script = do
    Env.script  .= script
    Env.timeout .= timeout
    updateLastActivityTime
    env <- use Env.lastActivityTime
    logger Logger.info  $ "Start request monitor time: " <> show env
    handleMessage

checkIntervalUs = 10 * 1000000

handleMessage :: StateT MonitorEnv BusT ()
handleMessage = do
    msgFrameE <- lift $ BusT $ Bus.withTimeout Bus.receive' checkIntervalUs
    case msgFrameE of
        Left err -> logger Logger.info  $ "Error. Retry."
        Right msgFrame ->
            case msgFrame of
                Left err -> logger Logger.error $ "Unparseable message: " <> err
                Right (MessageFrame msg crlID senderID lastFrame) -> do
                    let topic = msg ^. Message.topic
                    case Utils.lastPart '.' topic of
                        "request"  -> logMessage topic
                        _          -> return ()
    stop <- idleTimeCheck
    when stop $ do
        logger Logger.info  $ "Exiting"
        liftIO exitFailure
    handleMessage

logMessage :: String ->  StateT MonitorEnv BusT ()
logMessage topic = do
    time <- updateLastActivityTime
    logger Logger.info  $ "Request: " <> topic <> ", last activity time: " <> show time

updateLastActivityTime :: StateT MonitorEnv BusT Integer
updateLastActivityTime = do
    time <- round <$> liftIO getPOSIXTime
    Env.lastActivityTime .= time
    return time

idleTimeCheck :: StateT MonitorEnv BusT Bool
idleTimeCheck = do
    currentTime      <- round <$> liftIO getPOSIXTime
    lastActivityTime <- use Env.lastActivityTime
    timeout          <- use Env.timeout
    if currentTime - lastActivityTime > timeout
        then do
            script <- use Env.script
            logger Logger.info  $ "Timeout reached. Running script: " <> script
            liftIO $ system script
            return True
        else return False
