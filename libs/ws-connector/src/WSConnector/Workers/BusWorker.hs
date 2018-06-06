{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module WSConnector.Workers.BusWorker (start) where

import           Prologue                     hiding (fail)
import           Prelude                      (fail)
import           System.Log.MLogger

import           Control.Concurrent            (forkIO)
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Concurrent.MVar       as MVar

import           Control.Monad                (forever)
import           ZMQ.Bus.Bus                  (Bus)
import qualified ZMQ.Bus.Bus                  as Bus
import qualified ZMQ.Bus.Data.Flag            as Flag
import qualified ZMQ.Bus.Data.Message         as Message
import qualified ZMQ.Bus.Data.MessageFrame    as MessageFrame
import           ZMQ.Bus.EndPoint             (BusEndPoints)

import           WSConnector.Data.WSMessage   (WSMessage (..))

logger :: Logger
logger = getLogger $moduleName

relevantTopics :: [String]
relevantTopics =  ["empire."]

shouldPassToClient :: MessageFrame.MessageFrame -> Message.ClientID -> Bool
shouldPassToClient frame clientId = isNotSender where
    isNotSender      = senderId /= clientId
    senderId         = frame ^. MessageFrame.senderID

fromBus :: Unagi.InChan WSMessage -> MVar.MVar Message.ClientID -> Bus ()
fromBus chan idVar = do
    mapM_ Bus.subscribe relevantTopics
    senderAppId <- liftIO $ MVar.takeMVar idVar
    forever $ do
        frame <- Bus.receive
        liftIO $ when (shouldPassToClient frame senderAppId) $ do
            let msg = frame ^. MessageFrame.message
            logger info $ "Received from Bus: " <> (msg ^. Message.topic)
            Unagi.writeChan chan $ WebMessage (msg ^. Message.topic)
                                              (msg ^. Message.message)

dispatchMessage :: WSMessage -> Bus ()
dispatchMessage (WebMessage topic msg) = do
    logger info $ "Pushing to Bus: " <> topic
    void $ Bus.send Flag.Enable $ Message.Message topic msg
dispatchMessage _ = return ()

toBus :: Unagi.OutChan WSMessage -> MVar.MVar Message.ClientID -> Bus ()
toBus chan idVar = do
    myId <- Bus.getClientID
    liftIO $ MVar.putMVar idVar myId
    forever $ do
        msg <- liftIO $ Unagi.readChan chan
        dispatchMessage msg


eitherToM :: (Monad m, Show a) => Either a b -> m b
eitherToM = either (fail . show) return

eitherToM' :: (Monad m, Show a) => m (Either a b) -> m b
eitherToM' action = action >>= eitherToM

start :: BusEndPoints
      -> Unagi.InChan WSMessage
      -> Unagi.OutChan WSMessage
      -> IO ()
start busEndPoints fromBusChan toBusChan = do
    exchangeIdsVar <- MVar.newEmptyMVar
    forkIO $ eitherToM' $ Bus.runBus busEndPoints
        $ fromBus fromBusChan exchangeIdsVar
    forkIO $ eitherToM' $ Bus.runBus busEndPoints
        $ toBus   toBusChan   exchangeIdsVar
    return ()
