{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module ZMQ.Bus.RPC.Pipes where

import           Control.Concurrent        (forkIO)
import           Control.Monad             (forever)
import           Pipes                     ((>->))
import qualified Pipes
import qualified Pipes.Concurrent          as Pipes
import           Prelude                   (fail)
import           Prologue                  hiding (error, fail)

import           System.Log.MLogger
import qualified ZMQ.Bus.Bus               as Bus
import           ZMQ.Bus.Data.Flag         (Flag)
import           ZMQ.Bus.Data.Message      (Message)
import qualified ZMQ.Bus.Data.Message      as Message
import qualified ZMQ.Bus.Data.MessageFrame as MessageFrame
import           ZMQ.Bus.Data.Topic        (Topic)
import           ZMQ.Bus.EndPoint          (BusEndPoints)
import           ZMQ.Bus.Trans             (BusT (BusT))
import qualified ZMQ.Bus.Trans             as BusT



logger :: Logger
logger = getLogger $moduleName


produce :: Pipes.Producer (Message, Message.CorrelationID) BusT ()
produce = forever $ do
    frame <- lift $ BusT Bus.receive
    liftIO $ logger debug $ "Received request: " ++ (frame ^. MessageFrame.message . Message.topic)
    Pipes.yield (frame ^. MessageFrame.message, frame ^. MessageFrame.correlation)


consume :: Pipes.Consumer (Message, Message.CorrelationID, Flag) BusT ()
consume = forever $ do
    (msg, crl, flag) <- Pipes.await
    liftIO $ logger debug $ "Sending reply: " ++ (msg ^. Message.topic)
    void $ lift $ BusT $ Bus.reply crl flag msg


run :: BusEndPoints -> [Topic]
    -> IO (Pipes.Input  (Message, Message.CorrelationID),
           Pipes.Output (Message, Message.CorrelationID, Flag))
run endPoints topics = do
    (output1, input1) <- Pipes.spawn $ Pipes.bounded 1
    (output2, input2) <- Pipes.spawn $ Pipes.bounded 1
    let forkPipesThread fun = void $ forkIO $ eitherStringToM $ Bus.runBus endPoints $ do
                            mapM_ Bus.subscribe topics
                            BusT.runBusT $ Pipes.runEffect fun
    forkPipesThread $ produce >-> Pipes.toOutput output1
    forkPipesThread $ Pipes.fromInput input2 >-> consume
    return (input1, output2)

eitherStringToM :: Monad m => m (Either String b) -> m b
eitherStringToM action = action >>= either fail return
