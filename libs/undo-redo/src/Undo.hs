{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Undo where

import qualified Compress
import           Control.Lens
import           Control.Monad.State       (MonadState, StateT (StateT), forM, runStateT)
import           Data.Binary               (decode)
import qualified Data.Binary               as Bin
import           Data.ByteString           (null)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.UUID.Types           (UUID)
import           Data.UUID.V4              as UUID (nextRandom)
import           Handlers                  (handlersMap)
import qualified LunaStudio.API.Graph.Redo as RedoRequest
import qualified LunaStudio.API.Graph.Undo as UndoRequest
import qualified LunaStudio.API.Request    as Request
import           LunaStudio.API.Topic      (Topic)
import           Prologue                  hiding (null, throwM)
import           UndoState
import qualified System.Log.MLogger        as Logger
import qualified ZMQ.Bus.Bus               as Bus
import qualified ZMQ.Bus.Data.Flag         as Flag
import qualified ZMQ.Bus.Data.Message      as Message
import           ZMQ.Bus.Data.MessageFrame (MessageFrame (MessageFrame))
import qualified ZMQ.Bus.Data.MessageFrame as MessageFrame
import           ZMQ.Bus.EndPoint          (BusEndPoints)
import qualified ZMQ.Bus.Trans             as Bus

import qualified System.IO as IO



logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

topic :: Topic
topic = "empire."

withBus :: forall a. UndoPure a -> Undo a
withBus action = Undo $ StateT $ liftIO . runStateT (runUndo action)

print' :: (MonadIO m, Show a) => a -> m ()
print' a = liftIO $ print a >> IO.hFlush IO.stdout

run :: BusEndPoints -> IO (Either Bus.Error ((), UndoState))
run endPoints = do
    let state = UndoState [] [] []
    Bus.runBus endPoints $ do
            Bus.subscribe topic
            let runner = forever $ receiveAndHandleMessage `catchAll` print'
            Bus.runBusT $ runStateT (runUndo runner) state

run' :: UndoState -> UndoPure a -> IO (a, UndoState)
run' state undo' = runStateT (runUndo undo') state

receiveAndHandleMessage :: Undo ()
receiveAndHandleMessage = do
    msgFrame <- receiveMessage
    action <- withBus $ handleMessage $ msgFrame ^. MessageFrame.message
    for_ action $ \msg -> lift $ Bus.BusT $ sendMessage msg

pattern UndoRequestTopic :: Topic
pattern UndoRequestTopic <- "empire.undo.request"
pattern RedoRequestTopic :: Topic
pattern RedoRequestTopic <- "empire.redo.request"

handleMessage :: Message.Message -> UndoPure (Maybe Action)
handleMessage msg = do
    let topic'  = msg ^. Message.topic
        content = Compress.unpack $ msg ^. Message.message
    case topic' of
        UndoRequestTopic -> do
            let Request.Request _ undoGuiID (UndoRequest.Request _) = decode content
            case undoGuiID of
                Just guiID -> doUndo guiID
                Nothing    -> return Nothing
        RedoRequestTopic -> do
            let Request.Request _ redoGuiID (RedoRequest.Request _) = decode content
            case redoGuiID of
                Just guiID -> doRedo guiID
                Nothing    -> return Nothing
        _ -> do
            runMessageHandler topic' content
            return Nothing

receiveMessage :: Undo MessageFrame
receiveMessage = do
    frame <- Undo $ lift $ Bus.BusT Bus.receive
    case frame of
        MessageFrame msg _ _ _ -> do
            let emptyMsg = null $ msg ^. Message.message
            if emptyMsg then receiveMessage else return frame

checkGuiId :: GuiID -> UndoMessage -> Bool
checkGuiId guiID msg = case msg of UndoMessage x _ _ _ _ _ -> x == guiID

act :: Act -> UndoMessage -> Action
act action undoMessage = case action of
    ActUndo -> case undoMessage of (UndoMessage _ _ topicUndo msgUndo _ _) -> Action topicUndo msgUndo
    ActRedo -> case undoMessage of (UndoMessage _ _ _ _ topicRedo msgRedo) -> Action topicRedo msgRedo

doUndo :: MonadState UndoState m => UUID -> m (Maybe Action)
doUndo guiID = do
    maybeMsg <- uses undo $ List.find (checkGuiId guiID)
    forM maybeMsg $ \msg -> do
        redo %= (msg :)
        undo %= List.delete msg
        history %= (msg :) --FIXME reverse the order of undo-redo messages?
        return $ act ActUndo msg

doRedo :: MonadState UndoState m => UUID -> m (Maybe Action)
doRedo guiID = do
    maybeMsg <- uses redo $ List.find (checkGuiId guiID)
    forM maybeMsg $ \msg -> do
        undo %= (msg :)
        redo %= List.delete msg
        history %= (msg :)
        return $ act ActRedo msg

runMessageHandler :: String -> ByteString -> UndoPure ()
runMessageHandler topic' content = do
    let handler   = Map.findWithDefault doNothing topic' handlersMap
        doNothing _ = return ()
    void $ handler content


sendMessage :: Action -> Bus.Bus ()
sendMessage action = do
    uuid <- liftIO $ UUID.nextRandom
    void $ Bus.send Flag.Enable $ case action of
        Action topic' msg -> Message.Message topic' $ Compress.pack . Bin.encode $ Request.Request uuid Nothing msg
