{-# LANGUAGE UndecidableInstances #-}

module Bus.Framework.App where

import Prologue

import qualified Control.Monad.State.Layered  as State
import qualified Bus.Data.Config as Config
import qualified Control.Monad.Trans.Control as Control
import qualified Bus.Data.Environment as Environment
import qualified System.ZMQ4 as Zmq
import qualified Bus.Data.Message as Message
import qualified Data.Text.Encoding as Encoding

import Bus.Data.Environment (Environment (Environment))
import Bus.Data.Config (Config)
import Bus.Data.Message (Topic, Message (Message))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Base (MonadBase)
import Data.ByteString (ByteString)

type AppT m = State.StateT Environment m

type App = AppT IO

type MonadApp m = (MonadIO m, State.Getter Environment m)

withContext :: MonadBaseControl IO m => (Zmq.Context -> m a) -> m a
withContext = Control.liftBaseOp Zmq.withContext

withSocket :: (MonadBaseControl IO m, Zmq.SocketType a)
    => Zmq.Context -> a -> (Zmq.Socket a -> m b) -> m b
withSocket = Control.liftBaseOp .: Zmq.withSocket

withEnvironment :: (MonadBaseControl IO m, MonadIO m)
    => (Environment -> m a) -> m a
withEnvironment callback =
    withContext $ \ctx ->
        withSocket ctx Zmq.Pub $ \pubSocket ->
            withSocket ctx Zmq.Sub $ \subSocket ->
                callback $ Environment pubSocket subSocket

run :: forall m a . (MonadIO m, MonadBaseControl IO m)
    => Config -> AppT m a -> m a
run config action = withEnvironment $ \env -> do
    let pub = env ^. Environment.pubSocket
    let sub = env ^. Environment.subSocket
    liftIO $ do
        Zmq.connect pub $ config ^. Config.pubSocketAddress
        Zmq.connect sub $ config ^. Config.subSocketAddress
    State.evalT action env

runProxy :: MonadIO m => Config -> m ()
runProxy config = liftIO $ withContext $ \ctx ->
    withSocket ctx Zmq.XPub $ \pub ->
        withSocket ctx Zmq.XSub $ \sub -> do
            Zmq.bind sub $ config ^. Config.pubSocketAddress
            Zmq.bind pub $ config ^. Config.subSocketAddress
            Zmq.proxy sub pub Nothing

{-liftIO $ withEnvironment $ \env -> do-}
    {-let pub = env ^. Environment.pubSocket-}
        {-sub = env ^. Environment.subSocket-}
        {-proxyConfig = Config.toProxyConfig config-}
    {-Zmq.bind pub $ proxyConfig ^. Config.pubSocketAddress-}
    {-Zmq.bind sub $ proxyConfig ^. Config.subSocketAddress-}
    {-Zmq.proxy sub pub Nothing-}


subscribe :: MonadApp m => [Topic] -> m ()
subscribe topics = do
    subSocket <- view Environment.subSocket <$> State.get @Environment
    liftIO $ traverse_ (Zmq.subscribe subSocket . Message.encodeTopic) topics

unsubscribe :: MonadApp m => [Topic] -> m ()
unsubscribe topics = do
    subSocket <- view Environment.subSocket <$> State.get @Environment
    liftIO $ traverse_ (Zmq.unsubscribe subSocket . Message.encodeTopic) topics

sendMessage :: MonadApp m => Message -> m ()
sendMessage msg = do
    pubSocket <- view Environment.pubSocket <$> State.get @Environment
    liftIO $ Zmq.send pubSocket mempty (Message.encode msg)

send :: MonadApp m => Topic -> ByteString -> m ()
send = sendMessage .: Message

receive :: MonadApp m => m Message
receive = do
    subSocket <- view Environment.subSocket <$> State.get @Environment
    liftIO $ do
        print "will recv"
        recv <- Zmq.receive subSocket
        print "recvd"
        pure $ Message.decode recv
    {-liftIO $ Message.decode <$> Zmq.receive subSocket-}
