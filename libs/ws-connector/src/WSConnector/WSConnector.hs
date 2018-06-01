{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module WSConnector.WSConnector where

import           Prologue
import           System.Log.MLogger

import           Control.Concurrent            (forkIO)
import           Control.Concurrent.STM        (STM, atomically)
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad                 (forever)
import qualified Data.ByteString               as ByteString
import qualified Network.WebSockets            as WS

import           ZMQ.Bus.EndPoint              (BusEndPoints)

import           WSConnector.Data.WSFrame      (WSFrame (..), deserializeFrame, messages, serializeFrame)
import           WSConnector.Data.WSMessage    (ControlCode (..), WSMessage (..))
import qualified WSConnector.Workers.BusWorker as BusWorker
import qualified WSConnector.WSConfig          as WSConfig

logger :: Logger
logger = getLogger $moduleName

handleDisconnect :: WS.ConnectionException -> IO ()
handleDisconnect _ = logger info "User disconnected"

getFreshClientId :: TVar Int -> TVar Int -> STM Int
getFreshClientId clientCounter currentClient = do
    modifyTVar clientCounter (+ 1)
    newId <- readTVar clientCounter
    writeTVar currentClient newId
    return newId

application :: TVar Int -> TVar Int -> Int -> TChan WSMessage -> TChan WSMessage -> WS.ServerApp
application clientCounter currentClient pingTime toBusChan fromBusChan pending = do
    newId <- atomically $ getFreshClientId clientCounter currentClient
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn pingTime

    let welcomeMessage = serializeFrame $ WSFrame [ControlMessage Welcome]
    WS.sendBinaryData conn welcomeMessage

    fromBusListenChan <- atomically $ dupTChan fromBusChan

    forkIO $ fromWeb newId clientCounter conn toBusChan fromBusChan
    toWeb conn fromBusListenChan

whileActive :: Int -> TVar Int -> IO () -> IO ()
whileActive clientId currentClient action = do
    action
    whileActive clientId currentClient action

fromWebLoop :: Int -> TVar Int -> WS.Connection -> TChan WSMessage -> TChan WSMessage -> IO ()
fromWebLoop clientId currentClient conn chan wsChan = whileActive clientId currentClient $ do
    webMessage <- WS.receiveData conn
    let frame = deserializeFrame webMessage
    atomically $ mapM_ (writeTChan chan) $ frame ^. messages
    atomically $ mapM_ (writeTChan wsChan) $ frame ^. messages

fromWeb :: Int -> TVar Int -> WS.Connection -> TChan WSMessage -> TChan WSMessage -> IO ()
fromWeb clientId currentClient conn chan wsChan = do
    flip catch handleDisconnect $ fromWebLoop clientId currentClient conn chan wsChan
    let takeoverMessage = serializeFrame $ WSFrame [ControlMessage ConnectionTakeover]
    WS.sendBinaryData conn takeoverMessage
    WS.sendClose      conn takeoverMessage

toWeb :: WS.Connection -> TChan WSMessage -> IO ()
toWeb conn chan = flip catch handleDisconnect $ forever $ do
    msg <- atomically $ readTChan chan
    let webMessage = serializeFrame $ WSFrame [msg]
    WS.sendBinaryData conn webMessage

run :: BusEndPoints -> WSConfig.Config -> IO ()
run busEndPoints config = do
    toBusChan       <- atomically newTChan
    fromBusChan     <- atomically newBroadcastTChan
    clientCounter   <- atomically $ newTVar 0
    currentClient   <- atomically $ newTVar 0

    BusWorker.start busEndPoints fromBusChan toBusChan

    WS.runServer (config ^. WSConfig.host) (config ^. WSConfig.port) $ application clientCounter currentClient (config ^. WSConfig.pingTime) toBusChan fromBusChan
