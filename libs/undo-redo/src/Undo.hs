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
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Lazy              (toStrict,fromStrict)
import           Data.Binary                       (Binary, decode)
import qualified Data.Binary                       as Bin

import qualified Data.Map.Strict                   as Map
import           Data.Map.Strict                   (Map)
import           Data.Maybe
import           Data.Set                          hiding (map)
import           Prologue                          hiding (throwM)

import           Empire.API.Data.GraphLocation     (GraphLocation)
import           Empire.API.Data.Graph             (Graph)
import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.NodeMeta          (NodeMeta)
import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.AddSubgraph      as AddSubgraph
import qualified Empire.API.Graph.RemoveNodes      as RemoveNodes
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.UpdateNodeExpression as UpdateNodeExpression
import qualified Empire.API.Topic                  as Topic
import           Empire.API.Response               (Response (..))
import qualified Empire.API.Response               as Response
import qualified Empire.API.Request                as Request
import           Empire.API.Request                (Request (..))
import qualified Empire.API.Topic                  as T


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
import           ZMQ.Bus.RPC.RPC                   as RPC


data UndoMessage where
    UndoMessage :: (Binary undoReq, Binary redoReq) => undoReq -> redoReq -> UndoMessage

data History = History { _undo :: [UndoMessage]
                       , _redo :: [UndoMessage]
                         }

newtype UndoT t a = UndoT (StateT History (ReaderT BusEndPoints t) a)
    deriving (Applicative, Functor, Monad, MonadState History, MonadReader BusEndPoints, MonadIO, MonadThrow)

data Action = ActUndo | ActRedo

makeLenses ''History

type Handler = ByteString -> UndoT IO ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [  makeHandler handleAddSubgraphUdo
    -- , makeHandler handleAddNodeUndo
    -- , makeHandler handleRemoveNodes
    ]


makeHandler :: forall a. (Topic.MessageTopic a, Binary a) => (a -> UndoT IO ()) -> (String, Handler)
makeHandler h = (Topic.topic (undefined :: a), process) where
   process content = h request where request = decode . fromStrict $ content

-- handleAddNodeUndo :: AddNode.Response -> UndoT IO ()
-- handleAddNodeUndo (Response.Response _ (AddNode.Request location nodeType nodeMeta connectTo _) _ res) =
--     case res of
--         Response.Error  err -> return ()
--         Response.Ok node -> do
--             let nodeId  = node ^. Node.nodeId
--                 undoMsg = RemoveNodes.Request location [nodeId]
--                 redoMsg = AddNode.Request location nodeType nodeMeta connectTo $ Just (nodeId)
--             undo %= (UndoMessage undoMsg redoMsg :)

handleAddSubgraphUdo :: AddSubgraph.Response -> UndoT IO ()  --dodac result do pola addsubgraph response bo zwraca request z podmienionym id
handleAddSubgraphUdo (Response.Response  _ (AddSubgraph.Request location nodes connections) _ _ ) = do
    let ids = map (^. Node.nodeId) nodes
        undoMsg = RemoveNodes.Request location ids
        redoMsg = AddSubgraph.Request location nodes connections
    undo %= (UndoMessage undoMsg redoMsg :)

-- handleRemoveNodesUndo :: RemoveNodes.Response -> UndoT IO ()
-- handleRemoveNodesUndo (Response.Response _ (RemoveNodes.Request location noddesIds) (RemoveNodes.Inverse nodes connections) _)
--
empty :: UndoList
empty = UndoList [] []

runUndo :: BusEndPoints -> IO ()
runUndo endPoints = evalStateT collect Undo.empty
    where UndoT collect = collectEvents endPoints

collectEvents :: BusEndPoints -> UndoT IO ()
collectEvents endPoints = do
    msgFrame <- Bus.runBus endPoints $ Bus.receive'
    case (join msgFrame) of
        Right (MessageFrame msg corId senderId lastFrm) -> do
            let topic = msg ^. Message.topic
                userId = show senderId
                content = msg ^. Message.message
            case topic of
                "empire.undo" -> f endPoints
                "empire.redo" -> g endPoints
                "empire." -> do collectedMessage topic content

f :: BusEndPoints -> UndoT IO ()
f endPoints = do
    h <- uses undo head
    redo %= (h :)
    undo %= tail
    let msg = case h of UndoMessage undo _ -> undo
    void $ Bus.runBus endPoints $ sendMessage "undo" msg

g :: BusEndPoints -> UndoT IO ()
g endPoints = do
    h <- uses redo head
    undo %= (h :)
    redo %= tail
    let msg = h ^. redoAction
    void $ Bus.runBus endPoints $ sendMessage "redo" msg

collectedMessage :: String -> ByteString -> UndoT IO ()
collectedMessage topic content = do
    let handler   = Map.findWithDefault doNothing topic handlersMap
        doNothing _ = return ()
    void $ handler content


takeEndPointsAndRun :: Bus.Bus () -> UndoT IO ()
takeEndPointsAndRun action = do
    endPoints <- ask
    void $ Bus.runBus endPoints action

sendUndo :: UndoMessage -> Bus.Bus ()
sendUndo msg = sendMessage ActUndo "undo" msg

sendRedo :: UndoMessage -> Bus.Bus ()
sendRedo msg = sendMessage ActRedo "redo" msg

sendMessage :: Action -> String -> UndoMessage -> Bus.Bus ()
sendMessage action topic msg = void $ Bus.send Flag.Enable $ Message.Message topic $ case action of
    ActUndo -> case msg of UndoMessage u _ -> toStrict $ Bin.encode u
    ActRedo -> case msg of UndoMessage _ r -> toStrict $ Bin.encode r
