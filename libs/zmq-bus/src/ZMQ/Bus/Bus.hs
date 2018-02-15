{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZMQ.Bus.Bus where

import qualified Control.Concurrent              as Concurrent
import qualified Control.Concurrent.Async        as Async
import           Control.Error                   (ExceptT, hoistEither, runExceptT)
import           Control.Monad.State
import           Data.ByteString                 (ByteString)
import           System.ZMQ4.Monadic             (ZMQ)
import qualified System.ZMQ4.Monadic             as ZMQ

import qualified Debug.Trace                     as T
import           Prologue                        hiding (hoistEither, liftIO, trace)
import           System.Log.MLogger
import qualified ZMQ.Bus.Control.Handler.Methods as Methods
import           ZMQ.Bus.Data.Flag               (Flag)
import           ZMQ.Bus.Data.Message            (Message)
import qualified ZMQ.Bus.Data.Message            as Message
import           ZMQ.Bus.Data.MessageFrame       (MessageFrame (MessageFrame))
import qualified ZMQ.Bus.Data.MessageFrame       as MessageFrame
import           ZMQ.Bus.Data.Topic              (Topic)
import qualified ZMQ.Bus.Data.Topic              as Topic
import qualified ZMQ.Bus.EndPoint                as EP
import           ZMQ.Bus.Env                     (BusEnv (BusEnv))
import qualified ZMQ.Bus.Env                     as Env
import qualified ZMQ.RPC.Client                  as Client


logger :: Logger
logger = getLogger $moduleName


type Error = String


type Bus    a = forall z. Bus' z a
type Bus' z a = StateT (BusEnv z) (ExceptT Error (ZMQ z)) a


requestClientID :: EP.EndPoint -> ExceptT Error (ZMQ z) Message.ClientID
requestClientID addr = do
    socket <- lift $ ZMQ.socket ZMQ.Req
    lift $ ZMQ.connect socket addr
    let request = Methods.CreateID
    response <- Client.query socket request
    lift $ ZMQ.close socket
    pure $ Methods.clientID response


runBus :: MonadIO m => EP.BusEndPoints -> Bus a -> m (Either Error a)
runBus endPoints fun = ZMQ.runZMQ $ runExceptT $ do
    logger trace "Connecting to bus..."
    clientID   <- requestClientID $ EP.controlEndPoint endPoints
    subSocket  <- lift $ ZMQ.socket ZMQ.Sub
    pushSocket <- lift $ ZMQ.socket ZMQ.Push
    lift $ ZMQ.connect subSocket  $ EP.pubEndPoint  endPoints
    lift $ ZMQ.connect pushSocket $ EP.pullEndPoint endPoints
    logger trace "Connected to bus"
    fst <$> runStateT fun (BusEnv subSocket pushSocket clientID 0)


getClientID :: Bus Message.ClientID
getClientID = Env.clientID <$> get


getPushSocket :: (MonadState (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Push)
getPushSocket = Env.pushSocket <$> get


getSubSocket :: (MonadState (BusEnv z) f) => f (ZMQ.Socket z ZMQ.Sub)
getSubSocket = Env.subSocket <$> get


getNewRequestID :: Bus Message.RequestID
getNewRequestID = do
    s <- get
    let requestID = Env.requestID s
    put $ s { Env.requestID = requestID + 1 }
    pure requestID


reply :: Message.CorrelationID -> Flag -> Message -> Bus ()
reply crlID lastFrame msg = do
    clientID <- getClientID
    sendFrame $ MessageFrame msg crlID clientID lastFrame


send :: Flag -> Message -> Bus Message.CorrelationID
send lastFrame msg = do
    correlationID <- Message.CorrelationID <$> getClientID <*> getNewRequestID
    reply correlationID lastFrame msg
    pure correlationID


sendFrame :: MessageFrame -> Bus ()
sendFrame = sendByteString . MessageFrame.toByteString


receive :: Bus MessageFrame --fixme [SB] receive & receive' are doing the same thing, leave just one
receive = receive' >>= lift . hoistEither


receive' :: Bus (Either Error MessageFrame)
receive' = MessageFrame.fromByteString <$> receiveByteString


withTimeout :: Bus' z a -> Int -> Bus' z (Either Error a)
withTimeout action timeout = runExceptT $ do
    state' <- get
    T.trace (show timeout) $ pure ()
    task <- lift3 $ ZMQ.async $ runExceptT $ runStateT action state'
    wait <- liftIO $ Async.async $ do Concurrent.threadDelay timeout
                                      pure "Timeout reached"
    r <- liftIO $ Async.waitEitherCancel wait task
    (result, newState) <- hoistEither $ join r
    put newState
    pure result


sendByteString :: ByteString -> Bus ()
sendByteString msg = do
    push <- getPushSocket
    lift2 $ ZMQ.send push [] msg


receiveByteString :: Bus ByteString
receiveByteString = do
    sub <- getSubSocket
    bs <- lift2 $ ZMQ.receive sub
    pure bs


subscribe :: Topic -> Bus ()
subscribe topic = do
    sub <- getSubSocket
    lift2 $ ZMQ.subscribe sub $ Topic.toByteString topic


unsubscribe :: Topic -> Bus ()
unsubscribe topic = do
    sub <- getSubSocket
    lift2 $ ZMQ.unsubscribe sub $ Topic.toByteString topic
