{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module ZMQ.Bus.Logger.Logger where

import           Control.Monad             (forever)
import           Control.Monad.State       (StateT, evalStateT, get, put)
import qualified Data.Binary               as Bin
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (unpack)
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import qualified Data.Map.Strict           as Map
import qualified Data.Maybe                as Maybe
import qualified Data.Time.Clock           as Clock

import           Control.Monad.Except      (MonadError)
import           Prologue                  hiding (error)
import           System.Log.MLogger
import qualified ZMQ.Bus.Bus               as Bus
import qualified ZMQ.Bus.Data.Message      as Message
import           ZMQ.Bus.Data.MessageFrame (MessageFrame (MessageFrame))
import           ZMQ.Bus.Data.Topic        (Topic)
import           ZMQ.Bus.EndPoint          (BusEndPoints)
import           ZMQ.Bus.Logger.Env        (Env)
import qualified ZMQ.Bus.Logger.Env        as Env
import           ZMQ.Bus.RPC.Types         (Response (Response), Result (ErrorResult, Status), Value (Value))
import           ZMQ.Bus.Trans             (BusT (..))
import qualified ZMQ.Bus.Trans             as Bus



logger :: Logger
logger = getLogger $moduleName


run :: BusEndPoints -> [Topic] -> IO (Either Bus.Error ())
run ep topics = Bus.runBus ep $ do
    logger info $ "Subscribing to topics: " ++ show topics
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (forever logMessage) def

logMessage :: StateT Env BusT ()
logMessage = do
    msgFrame <- lift $ Bus.BusT Bus.receive'
    case msgFrame of
        Left err -> logger error $ "Unparseable message: " ++ err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            time <- measureTime crlID
            let topic = msg ^. Message.topic
                logMsg =  show senderID
                       ++ " -> "
                       ++ " (last = "
                       ++ show lastFrame
                       ++ ")"
                       ++ "\t:: "
                       ++ topic
                       ++ Maybe.maybe  "" (\t -> " [" ++ show t ++ "]") time
                content = msg ^. Message.message
                errorMsg = show content
            case lastPart '.' topic of
                "response" -> do logger info  logMsg
                                 lift $ BusT $ lift $ ppr content
                "status"   -> logger info  logMsg
                "update"   -> logger info  logMsg
                "request"  -> logger info  logMsg
                _          -> do logger error logMsg
                                 logger error errorMsg


ppr :: (MonadIO m, MonadError String m)
    => ByteString -> m ()
ppr msg = do
    --Request fname (Value typeName protocol dataBytes) <- hoistEither $ RPC.messageGet' msg
    let Response fname result _ = Bin.decode $ fromStrict msg
    case result of
        (Status (Value tname _ dataBytes)) -> do
            logger info  $ unlines [ "requested method: " ++ fname
                                   , "    content type: " ++ tname
                                   ]
            logger debug $ unlines [ "            data: "
                                   , unpack $ toStrict dataBytes
                                   ]
        (ErrorResult err) ->
            logger error $ unlines [ "requested method: " ++ fname
                                   , "  error response: " ++ err
                                   ]


lastPart :: Eq a => a -> [a] -> [a]
lastPart = lastPartIntern []

lastPartIntern :: Eq a => [a] -> a -> [a] -> [a]
lastPartIntern _      b (a:as) | a == b = lastPartIntern [] b as
lastPartIntern buffer _ []              = reverse buffer
lastPartIntern buffer b (a:as)          = lastPartIntern (a:buffer) b as

measureTime :: MonadIO m => Message.CorrelationID -> StateT Env m (Maybe Clock.NominalDiffTime)
measureTime !crlID = do
    stop  <- liftIO Clock.getCurrentTime
    times <- get
    put $! Env.times %~ Map.insert crlID stop $! times
    return $ fmap (Clock.diffUTCTime stop) $ Map.lookup crlID $ times ^. Env.times
