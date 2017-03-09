{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module ZMQ.Bus.RPC.Server.Processor where

import           Control.Error             (ExceptT, exceptT, hoistEither)
import           Control.Monad             (liftM)
import qualified Control.Monad.Catch       as Catch
import           Control.Monad.Trans.State (StateT)
import           Data.Binary               (Binary)
import           Data.Either               as Either
import qualified Data.Maybe                as Maybe

import           Prologue                  hiding (error, hoistEither)
import           System.Log.MLogger
import           ZMQ.Bus.Data.Message      (CorrelationID, Message)
import qualified ZMQ.Bus.Data.Message      as Message
import           ZMQ.Bus.Data.Topic        ((/+))
import           ZMQ.Bus.RPC.HandlerMap    (HandlerMap)
import qualified ZMQ.Bus.RPC.HandlerMap    as HandlerMap
import qualified ZMQ.Bus.RPC.RPC           as RPC
import           ZMQ.Bus.RPC.Types



logger :: Logger
logger = getLogger $moduleName


singleResult :: MonadIO m => (a -> m b) -> a -> m [b]
singleResult f a = liftM return $ f a


noResult :: MonadIO m => (a -> m ()) -> a -> m [Response]
noResult f a = f a >> return []


-- FIXME: typ malo mowi
optResult :: MonadIO m => (a -> m (Maybe b)) -> a -> m [b]
optResult f a = liftM Maybe.maybeToList $ f a


process :: forall s m. (Catch.MonadCatch m, MonadIO m)
        => HandlerMap s m -> CorrelationID -> Message -> StateT s m [Message]
process handlerMap correlationID msg = either handleError (\message -> do
        m <- message
        logger debug $ show m
        return m
    ) handleMessage
    where
        call :: HandlerMap.Callback s m
        call method = exceptT errorHandler applyArgs deserializeMsg
            where
                deserializeMsg :: (Binary a, Typeable a) => ExceptT String (StateT s m) a
                deserializeMsg = do
                    req    <- hoistEither request
                    unpackValue $ req ^. arguments
                errorHandler err = do logger error err
                                      return (ErrorResult err, [])
                applyArgs desReq = do
                    status <- RPC.run $ method correlationID desReq
                    case status of
                        Left err -> do logger error err
                                       return (ErrorResult err, [])
                        Right (res, update) -> return (Status $ packValue res, update)
        mkResponse :: Either String ((Result, [Value]) -> Response)
        mkResponse = make <$> functionName
            where make fname (result', updates) = Response fname result' updates
        respond :: Response -> [Message]
        respond resp = [Message.mk ((msg ^. Message.topic) /+ "response") resp]
        functionName :: Either String FunctionName
        functionName = (^. requestMethod) <$> request
        request :: Either String Request
        request = RPC.messageGet' $ msg ^. Message.message

        handleError :: String -> StateT s m [Message]
        handleError s = return $ respond $ Response fname (ErrorResult s) []
                where fname = either (const "") id functionName

        handleMessage :: Either String (StateT s m [Message])
        handleMessage = do
            let hmap = HandlerMap.lookupAndCall handlerMap call :: FunctionName -> StateT s m (Result, [Value])
            f <- mkResponse
            (fmap . fmap) (respond . f) $ hmap . functionName
