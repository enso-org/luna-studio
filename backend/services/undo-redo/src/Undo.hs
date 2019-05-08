{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Undo where

import Prologue

import qualified Bus.Data.Message          as Message
import qualified Bus.Framework.App         as Bus
import qualified Compress
import qualified Data.Binary               as Bin
import qualified Data.ByteString           as ByteString
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import qualified Data.UUID.V4              as UUID
import qualified LunaStudio.API.Graph.Redo as RedoRequest
import qualified LunaStudio.API.Graph.Undo as UndoRequest
import qualified LunaStudio.API.Request    as Request
import qualified System.IO                 as IO
import qualified System.Log.MLogger        as Logger

import Bus.Data.Config      (Config)
import Bus.Data.Message     (Message)
import Control.Lens         (uses, (%=))
import Control.Monad.State  (MonadState, StateT (StateT), evalStateT, runStateT)
import Data.Binary          (decode)
import Data.ByteString.Lazy (ByteString)
import Data.UUID.Types      (UUID)
import Handlers             (handlersMap)
import LunaStudio.API.Topic (Topic)
import UndoState

logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)

topic :: Topic
topic = "empire."

withBus :: forall a. UndoPure a -> Undo a
withBus action = Undo $ StateT $ liftIO . runStateT (runUndo action)

print' :: (MonadIO m, Show a) => a -> m ()
print' a = liftIO $ print a >> IO.hFlush IO.stdout

run :: Config -> IO UndoState
run endPoints = do
    let state = UndoState [] [] []
    Bus.run endPoints $ do
            Bus.subscribe [topic]
            let runner = forever $ receiveAndHandleMessage `catchAll` print'
            evalStateT (runUndo runner) state

run' :: UndoState -> UndoPure a -> IO (a, UndoState)
run' state undo' = runStateT (runUndo undo') state

receiveAndHandleMessage :: Undo ()
receiveAndHandleMessage = do
    msg    <- receiveMessage
    action <- withBus $ handleMessage msg
    for_ action $ \msg -> lift $ sendMessage msg

pattern UndoRequestTopic :: Topic
pattern UndoRequestTopic <- "empire.undo.request"
pattern RedoRequestTopic :: Topic
pattern RedoRequestTopic <- "empire.redo.request"

handleMessage :: Message -> UndoPure (Maybe Action)
handleMessage msg = do
    let topic'  = msg ^. Message.topic
        content = Compress.unpack $ msg ^. Message.body
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

receiveMessage :: Undo Message
receiveMessage = do
    message <- lift Bus.receive
    let emptyMsg = ByteString.null $ message ^. Message.body
    if emptyMsg then receiveMessage else return message

checkGuiId :: GuiID -> UndoMessage -> Bool
checkGuiId guiID msg = case msg of UndoMessage x _ _ _ _ _ -> x == guiID

act :: Act -> UndoMessage -> Action
act action undoMessage = case action of
    ActUndo -> case undoMessage of (UndoMessage _ _ topicUndo msgUndo _ _) -> Action topicUndo msgUndo
    ActRedo -> case undoMessage of (UndoMessage _ _ _ _ topicRedo msgRedo) -> Action topicRedo msgRedo

doUndo :: MonadState UndoState m => UUID -> m (Maybe Action)
doUndo guiID = do
    maybeMsg <- uses undo $ List.find (checkGuiId guiID)
    for maybeMsg $ \msg -> do
        redo %= (msg :)
        undo %= List.delete msg
        history %= (msg :) --FIXME reverse the order of undo-redo messages?
        return $ act ActUndo msg

doRedo :: MonadState UndoState m => UUID -> m (Maybe Action)
doRedo guiID = do
    maybeMsg <- uses redo $ List.find (checkGuiId guiID)
    for maybeMsg $ \msg -> do
        undo %= (msg :)
        redo %= List.delete msg
        history %= (msg :)
        return $ act ActRedo msg

runMessageHandler :: String -> ByteString -> UndoPure ()
runMessageHandler topic' content = do
    let handler   = Map.findWithDefault doNothing topic' handlersMap
        doNothing _ = return ()
    void $ handler content


sendMessage :: Action -> Bus.App ()
sendMessage action = do
    uuid <- liftIO UUID.nextRandom
    void . Bus.sendMessage $ case action of
        Action topic' msg -> Message.Message topic' $ Compress.pack . Bin.encode $ Request.Request uuid Nothing msg
