{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module WSConnector.Workers.BusWorker (start) where

import           Prologue
import           System.Log.MLogger

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
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

-- shouldPassToClient frame clientId = isOriginalAuthor && isNotSender where
--     isOriginalAuthor = originalAuthorId == clientId
--     originalAuthorId = frame ^. MessageFrame.correlation . Message.clientID
--     isNotSender      = senderId /= clientId
--     senderId         = frame ^. MessageFrame.senderID

fromBus :: TChan WSMessage -> TChan Message.ClientID -> Bus ()
fromBus chan idChan = do
    mapM_ Bus.subscribe relevantTopics
    senderAppId <- liftIO $ atomically $ readTChan idChan
    forever $ do
        frame <- Bus.receive
        when (shouldPassToClient frame senderAppId) $ do
            let msg = frame ^. MessageFrame.message
            logger info $ "Received from Bus: " ++ (show msg)
            liftIO $ atomically $ writeTChan chan $ WebMessage (msg ^. Message.topic)
                                                               (msg ^. Message.message)

dispatchMessage :: WSMessage -> Bus ()
dispatchMessage (WebMessage topic msg) = do
    logger info $ "Pushing to Bus: " ++ (show msg)
    void $ Bus.send Flag.Enable $ Message.Message topic msg
dispatchMessage _ = return ()

toBus :: TChan WSMessage -> TChan Message.ClientID -> Bus ()
toBus chan idChan = do
    myId <- Bus.getClientID
    liftIO $ atomically $ writeTChan idChan myId
    forever $ do
        msg <- liftIO $ atomically $ readTChan chan
        dispatchMessage msg


eitherToM :: (Monad m, Show a) => Either a b -> m b
eitherToM = either (fail . show) return

eitherToM' :: (Monad m, Show a) => m (Either a b) -> m b
eitherToM' action = action >>= eitherToM

start :: BusEndPoints -> TChan WSMessage -> TChan WSMessage -> IO ()
start busEndPoints fromBusChan toBusChan = do
    exchangeIdsChan <- atomically newTChan
    forkIO $ eitherToM' $ Bus.runBus busEndPoints $ fromBus fromBusChan exchangeIdsChan
    forkIO $ eitherToM' $ Bus.runBus busEndPoints $ toBus   toBusChan   exchangeIdsChan
    return ()
