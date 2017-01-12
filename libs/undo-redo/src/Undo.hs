{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Undo where

import           Control.Exception                (Exception)
import           Control.Exception.Safe           (MonadThrow, throwM)
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.STM                 (atomically)
import           Data.ByteString                   (ByteString, empty)
import           Data.ByteString.Lazy              (toStrict,fromStrict)
import           Data.Binary                       (Binary, decode)
import qualified Data.Binary                       as Bin
import qualified Data.Text.Lazy                        as Text
import qualified Data.List as List
import qualified Data.Map.Strict                   as Map
import           Data.Map.Strict                   (Map)
import           Data.Maybe
import qualified Data.Set                          as Set
import           Data.UUID.Types                       (UUID)
import           Prologue                          hiding (throwM)
import Util as Util

import           Data.UUID as UUID (nil)
import           Data.UUID.V4 as UUID (nextRandom)
import           Empire.API.Data.Connection        (Connection)
import           Empire.API.Data.Connection        as Connection
import           Empire.API.Data.GraphLocation     (GraphLocation)
import           Empire.API.Data.Graph             (Graph)
import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.NodeMeta          (NodeMeta)
import           Empire.API.Data.PortRef (InPortRef, OutPortRef, dstNodeId, srcNodeId)
import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.AddSubgraph      as AddSubgraph
import qualified Empire.API.Graph.Connect              as Connect
import qualified Empire.API.Graph.Disconnect           as Disconnect
import qualified Empire.API.Data.PortRef           as PortRef
import qualified Empire.API.Graph.RemoveNodes      as RemoveNodes
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.Undo             as UndoRequest
import qualified Empire.API.Graph.Redo             as RedoRequest
import qualified Empire.API.Graph.UpdateNodeExpression as UpdateNodeExpression
import           Empire.API.Graph.UpdateNodeMeta   (SingleUpdate)
import qualified Empire.API.Graph.UpdateNodeMeta   as UpdateNodeMeta
import qualified Empire.API.Topic                  as Topic
import           Empire.API.Response               (Response (..))
import qualified Empire.API.Response               as Response
import qualified Empire.API.Request                as Request
import           Empire.API.Request                (Request (..))

import           Empire.Env                        (Env)
import qualified Empire.Env                        as Env
import qualified Empire.Server.Graph               as Graph
import           Empire.Server.Server              (sendToBus')

import qualified ZMQ.Bus.Bus                       as Bus
import qualified ZMQ.Bus.Data.Flag                 as Flag
import qualified ZMQ.Bus.Data.Message              as Message
import           ZMQ.Bus.Data.MessageFrame         (MessageFrame (MessageFrame))
import           ZMQ.Bus.EndPoint                  (BusEndPoints)
import qualified ZMQ.Bus.Trans                     as Bus
import qualified ZMQ.Bus.Data.MessageFrame       as MessageFrame
import qualified ZMQ.RPC.Client                  as Client
import           Control.Error                   (ExceptT, hoistEither, runExceptT)

import System.IO (stdout,hFlush)

type GuiID   = UUID
type ReqUUID = UUID
data UndoMessage where
    UndoMessage :: (Binary undoReq, Binary redoReq) => GuiID -> ReqUUID -> Topic.Topic -> undoReq -> Topic.Topic -> redoReq -> UndoMessage

instance Eq UndoMessage where
    (UndoMessage _ reqID1 _ _ _ _) == (UndoMessage _ reqID2 _ _ _ _) = (reqID1 == reqID2)
instance Show UndoMessage where
    show (UndoMessage guiID reqID topic1 _ topic2 _) =
        "UndoMessage " ++ show guiID ++ " " ++ show reqID ++ " " ++ show topic1 ++ " " ++ show topic2

-- FIXME[WD]: nie uzywajmy NIGDY exystencjali
-- FIXME[WD]: uzywajmy lensow
-- data UndoMessage = forall undoReq redoReq. (Binary undoReq, Binary redoReq) => UndoMessage GuiID Topic.Topic undoReq Topic.Topic redoReq UndoMessage
--
-- data UndoMessage = UndoMessage GuiID Topic.Topic UndoReq Topic.Topic RedoReq UndoMessage
-- newtype UndoReq = UndoReq ByteString
-- newtype RedoReq = RedoReq ByteString
--

----------------------
-- === Handlers === --
----------------------

data UndoState = UndoState { _undo    :: [UndoMessage]
                           , _redo    :: [UndoMessage]
                           , _history :: [UndoMessage]
                           }
makeLenses ''UndoState
-- === Utils === --

-- FIXME[WD]: Undo -> History?
newtype Undo a = Undo {runUndo :: StateT UndoState (Bus.BusT) a}
    deriving (Applicative, Functor, Monad, MonadState UndoState, MonadIO, MonadThrow)

type Undo = Undo' Bus.BusT
type UndoPure = Undo' IO

lft :: UndoPure a -> Undo a
lft act = Undo $ state $ runStateT actUndo
    where actUndo = runUndo act


run :: BusEndPoints -> IO (Either Bus.Error ((), UndoState))
run endPoints = run' endPoints $ forever receiveAndHandleMessage

run' :: BusEndPoints -> Undo a -> IO (Either Bus.Error (a, UndoState))
run' endPoints undo = do
    Bus.runBus endPoints $ do
        Bus.subscribe "empire."
        let state = UndoState [] [] []
        Bus.runBusT $ runStateT (runUndo undo) state

receiveAndHandleMessage :: Undo ()
receiveAndHandleMessage = do
    msgFrame <- receiveMessage
    handleMessage $ msgFrame ^. MessageFrame.message

handleMessage :: Message.Message -> UndoPure ()
handleMessage msg = do
    let topic   = msg ^. Message.topic
        content = msg ^. Message.message
        Request.Request _ guiID (RedoRequest.Request _) = decode . fromStrict $ content

    flushHelper $ show topic
    flushHelper $ show guiID
    case topic of
        "empire.undo.request" -> do
            req <- doUndo guiID
            case req of
                Just msg -> Undo $ lift $ Bus.BusT $ sendUndo msg
                Nothing  -> return ()
        "empire.redo.request" -> do
            req <- doRedo guiID
            case req of
                Just msg -> Undo $ lift $ Bus.BusT $ sendRedo msg
                Nothing  -> return ()
        _ -> if (guiID /= Nothing) then collectedMessage topic content else return ()

isEmpty :: ByteString -> Bool
isEmpty msg = empty == msg

receiveMessage :: Undo MessageFrame
receiveMessage = do
    frame <- Undo $ lift $ Bus.BusT Bus.receive
    case frame of
        MessageFrame msg _ _ _ -> do
            let emptyMsg = isEmpty $ msg ^. Message.message
            if emptyMsg then receiveMessage else return frame

compareId :: UUID -> UndoMessage -> Bool
compareId guiID msg = case msg of UndoMessage x _ _ _ _ _ -> x == guiID



doUndo :: MonadState UndoState m => Maybe UUID -> m (Maybe UndoMessage)
doUndo guiID = do
    let justId = fromJust guiID
    h <- uses undo $ List.find (compareId justId)
    case h of
        -- FIXME: to jest MapM?
        Just msg -> do redo %= (msg :)
                       undo %= List.delete msg
                       history %= (msg :) --FIXME odwróć kolejność wiadomości undo-redo?
                       return $ Just msg
        Nothing  -> return Nothing

doRedo :: MonadState UndoState m => Maybe UUID -> m (Maybe UndoMessage)
doRedo guiID = do
    let justId = fromJust guiID
    h <- uses redo $ List.find (compareId justId)
    case h of
        Just msg -> do undo %= (msg :)
                       redo %= List.delete msg
                       history %= (msg :)
                       return $ Just msg
        Nothing  -> return Nothing

collectedMessage :: String -> ByteString -> Undo ()
collectedMessage topic content = do
    let handler   = Map.findWithDefault doNothing topic handlersMap
        doNothing _ = return ()
    void $ handler content

-- FIXME[WD]: ActX -> X
data Action = ActUndo
            | ActRedo


sendUndo :: UndoMessage -> Bus.Bus ()
sendUndo msg = sendMessage ActUndo msg

sendRedo :: UndoMessage -> Bus.Bus ()
sendRedo msg = sendMessage ActRedo msg

sendMessage :: Action -> UndoMessage -> Bus.Bus ()
sendMessage action msg = do
    uuid <- liftIO $ UUID.nextRandom
    void $ Bus.send Flag.Enable $ case action of
        ActUndo -> case msg of UndoMessage _ _ tu u _ _ -> Message.Message tu $ toStrict $ Bin.encode $ Request.Request uuid Nothing u
        ActRedo -> case msg of UndoMessage _ _ _ _ tr r -> Message.Message tr $ toStrict $ Bin.encode $ Request.Request uuid Nothing r
