{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Undo where

import Handlers (handlersMap)
import UndoState

import           Control.Exception                 (Exception)
import           Control.Exception.Safe            (MonadThrow, throwM)
import           Control.Lens
import           Control.Monad.State               hiding (when)
import           Control.Monad.STM                 (atomically)
import           Data.ByteString                   (ByteString, null)
import           Data.ByteString.Lazy              (toStrict,fromStrict)
import           Data.Binary                       (Binary, decode)
import qualified Data.Binary                       as Bin
import qualified Data.Text.Lazy                    as Text
import qualified Data.List                         as List
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import qualified Data.Set                          as Set
import           Data.UUID.Types                   (UUID)
import           Prologue                          hiding (throwM, null)
import           Util                              as Util

import           Data.UUID as UUID (nil)
import           Data.UUID.V4 as UUID (nextRandom)
import qualified Empire.API.Topic                  as Topic
import           Empire.API.Response               (Response (..))
import qualified Empire.API.Response               as Response
import qualified Empire.API.Request                as Request
import           Empire.API.Request                (Request (..))
import qualified Empire.API.Graph.Undo             as UndoRequest
import qualified Empire.API.Graph.Redo             as RedoRequest

import qualified ZMQ.Bus.Bus                       as Bus
import qualified ZMQ.Bus.Data.Flag                 as Flag
import qualified ZMQ.Bus.Data.Message              as Message
import           ZMQ.Bus.Data.MessageFrame         (MessageFrame (MessageFrame))
import           ZMQ.Bus.EndPoint                  (BusEndPoints)
import qualified ZMQ.Bus.Trans                     as Bus
import qualified ZMQ.Bus.Data.MessageFrame         as MessageFrame
import           Control.Error                     (ExceptT, hoistEither, runExceptT)

topic = "empire."

withBus :: forall a. UndoPure a -> Undo a
withBus act = Undo $ StateT $ \s -> liftIO $ runStateT (runUndo act) s


run :: BusEndPoints -> IO (Either Bus.Error ((), UndoState))
run endPoints = do
    let state = UndoState [] [] []
    run' endPoints state $ forever receiveAndHandleMessage

run' :: BusEndPoints -> UndoState -> Undo a -> IO (Either Bus.Error (a, UndoState))
run' endPoints state undo = Bus.runBus endPoints $ do
        Bus.subscribe topic
        Bus.runBusT $ runStateT (runUndo undo) state

receiveAndHandleMessage :: Undo ()
receiveAndHandleMessage = do
    msgFrame <- receiveMessage
    action <- withBus $ handleMessage $ msgFrame ^. MessageFrame.message
    forM_ action $ \msg -> lift $ Bus.BusT $ sendMessage msg

pattern UndoRequestTopic <- "empire.undo.request"
pattern RedoRequestTopic <- "empire.redo.request"

handleMessage :: Message.Message -> UndoPure (Maybe Action)
handleMessage msg = do
    let topic   = msg ^. Message.topic
        content = msg ^. Message.message
    case topic of
        UndoRequestTopic -> do
            let Request.Request _ undoGuiID (UndoRequest.Request _) = decode . fromStrict $ content
            case undoGuiID of
                Just guiID -> doUndo guiID
                Nothing    -> return Nothing
        RedoRequestTopic -> do
            let Request.Request _ redoGuiID (RedoRequest.Request _) = decode . fromStrict $ content
            case redoGuiID of
                Just guiID -> doRedo guiID
                Nothing    -> return Nothing
        _ -> do
            runMessageHandler topic content
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
        history %= (msg :) --FIXME odwróć kolejność wiadomości undo-redo?
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
runMessageHandler topic content = do
    let handler   = Map.findWithDefault doNothing topic handlersMap
        doNothing _ = return ()
    void $ handler content


sendMessage :: Action -> Bus.Bus ()
sendMessage action = do
    uuid <- liftIO $ UUID.nextRandom
    void $ Bus.send Flag.Enable $ case action of
        Action topic msg -> Message.Message topic $ toStrict $ Bin.encode $ Request.Request uuid Nothing msg