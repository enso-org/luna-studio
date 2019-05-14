{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module WSConnector.Workers.BusWorker where

import Prologue hiding (fail)

import qualified Bus.Data.Config               as Bus
import qualified Bus.Data.Message              as Message
import qualified Bus.Framework.App             as Bus
import qualified Control.Concurrent.Chan.Unagi as Unagi

import Control.Concurrent         (forkIO)
import Control.Monad              (forever)
import Prelude                    (fail)
import System.Log.MLogger
import WSConnector.Data.WSMessage (WSMessage (..))

logger :: Logger
logger = getLogger $moduleName

relevantTopics :: [String]
relevantTopics =  ["empire."]

fromBus :: Unagi.InChan WSMessage -> Bus.App ()
fromBus chan = do
    Bus.subscribe relevantTopics
    forever $ do
        msg <- Bus.receive
        liftIO $ do
            logger info $ "Received from Bus: " <> (msg ^. Message.topic)
            Unagi.writeChan chan $ WebMessage (msg ^. Message.topic)
                                              (msg ^. Message.body)

dispatchMessage :: WSMessage -> Bus.App ()
dispatchMessage (WebMessage topic msg) = do
    logger info $ "Pushing to Bus: " <> topic
    void $ Bus.send topic msg
dispatchMessage _ = return ()

toBus :: Unagi.OutChan WSMessage -> Bus.App ()
toBus chan = forever $ do
    msg <- liftIO $ Unagi.readChan chan
    dispatchMessage msg

eitherToM :: (Monad m, Show a) => Either a b -> m b
eitherToM = either (fail . show) return

eitherToM' :: (Monad m, Show a) => m (Either a b) -> m b
eitherToM' action = action >>= eitherToM

start :: Bus.Config
      -> Unagi.InChan WSMessage
      -> Unagi.OutChan WSMessage
      -> IO ()
start busEndPoints fromBusChan toBusChan = do
    forkIO $ Bus.run busEndPoints $ fromBus fromBusChan
    forkIO $ Bus.run busEndPoints $ toBus   toBusChan
    return ()
