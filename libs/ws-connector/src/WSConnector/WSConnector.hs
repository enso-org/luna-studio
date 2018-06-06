{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns    #-}

module WSConnector.WSConnector where

import           Prologue
import           System.Log.MLogger

import           Control.Concurrent            (forkIO)
import qualified Control.Concurrent.MVar       as MVar
import qualified Control.Concurrent.Chan.Unagi as Unagi
import           Control.Monad                 (forever)
import qualified Data.ByteString               as ByteString
import qualified Network.WebSockets            as WS

import           ZMQ.Bus.EndPoint              (BusEndPoints)

import           WSConnector.Data.WSFrame      (WSFrame (..), deserializeFrame, messages, serializeFrame)
import           WSConnector.Data.WSMessage    (ControlCode (..), WSMessage (..))
import qualified WSConnector.Workers.BusWorker as BusWorker
import qualified WSConnector.WSConfig          as WSConfig

import qualified System.CPUTime                as CPUTime

logger :: Logger
logger = getLogger $moduleName

timed :: MonadIO m => m a -> m Double
timed act = do
    t0 <- liftIO $ CPUTime.getCPUTime
    act
    t1 <- liftIO $ CPUTime.getCPUTime
    return $ fromIntegral (t1 - t0) * 1e-12

handleDisconnect :: WS.ConnectionException -> IO ()
handleDisconnect _ = logger info "User disconnected"

fromWebApp :: Int -> Unagi.InChan WSMessage -> WS.ServerApp
fromWebApp pingTime toBusChan pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn pingTime

    fromWeb conn toBusChan

toWebApp :: Int -> Unagi.InChan WSMessage -> WS.ServerApp
toWebApp pingTime fromBusChan pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn pingTime

    let welcomeMessage = serializeFrame $ WSFrame [ControlMessage Welcome]
    WS.sendBinaryData conn welcomeMessage

    fromBusListenChan <- Unagi.dupChan fromBusChan
    toWeb conn fromBusListenChan

fromWebLoop :: WS.Connection -> Unagi.InChan WSMessage -> IO ()
fromWebLoop conn chan = forever $ do
    webMessage <- WS.receiveData conn
    let frame = deserializeFrame webMessage
    mapM_ (Unagi.writeChan chan) $ frame ^. messages

fromWeb :: WS.Connection -> Unagi.InChan WSMessage -> IO ()
fromWeb conn chan = do
    flip catch handleDisconnect $ fromWebLoop conn chan
    let closeMsg = serializeFrame $ WSFrame [ControlMessage ConnectionTakeover]
    WS.sendClose conn closeMsg

toWebLoop :: WS.Connection -> Unagi.OutChan WSMessage -> IO ()
toWebLoop conn chan = forever $ do
    msg <- Unagi.readChan chan
    let !webMessage = serializeFrame $ WSFrame [msg]
        len = ByteString.length webMessage
    logger info $ "Sending " <> show len <> " bytes."
    t <- timed $ WS.sendBinaryData conn webMessage
    let mbps = floor $ fromIntegral len * 1e-6 / t
    logger info $ "Sent " <> show len <> " bytes at " <> show mbps <> " MB/s."

toWeb :: WS.Connection -> Unagi.OutChan WSMessage -> IO ()
toWeb conn chan = do
    flip catch handleDisconnect $ toWebLoop conn chan
    let takeoverMessage = serializeFrame $ WSFrame [ControlMessage ConnectionTakeover]
    WS.sendClose conn takeoverMessage

sinkChan :: Unagi.OutChan a -> IO ()
sinkChan c = forever $ do
    !_ <- Unagi.readChan c
    return ()

run :: BusEndPoints -> WSConfig.Config -> IO ()
run busEndPoints config = do
    (toBusIn, toBusOut)     <- Unagi.newChan
    (fromBusIn, fromBusOut) <- Unagi.newChan

    BusWorker.start busEndPoints fromBusIn toBusOut

    let host        = config ^. WSConfig.host
        fromWebPort = config ^. WSConfig.fromWebPort
        toWebPort   = config ^. WSConfig.toWebPort
        pingTime    = config ^. WSConfig.pingTime

    -- We need this to flush data from the unused end of this channel.
    -- Actual read ends are created with `Unagi.dupChan`, per-client.
    forkIO $ sinkChan fromBusOut

    forkIO $ WS.runServer host fromWebPort $ fromWebApp pingTime toBusIn
    WS.runServer host toWebPort $ toWebApp pingTime fromBusIn
