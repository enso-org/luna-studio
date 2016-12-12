{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Undo where


import           Control.Concurrent.STM.TChan (writeTChan)

import Control.Monad.State
import Control.Lens
import Control.Exception.Base
import           Control.Monad.STM                (atomically)
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Lazy                   (toStrict,fromStrict)
import Data.Binary (Binary, decode)
import qualified Data.Binary                  as Bin

import qualified Data.Map.Strict       as Map
import           Data.Map.Strict       (Map)
import Data.Maybe
import Data.Set
import Prelude

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Graph         (Graph)
import           Empire.API.Data.Node          (Node, NodeId)
import qualified Empire.API.Data.Node          as Node
import           Empire.API.Data.NodeMeta      (NodeMeta)
import qualified Empire.API.Graph.AddNode      as AddNode
import qualified Empire.API.Graph.RemoveNodes  as RemoveNodes
import qualified Empire.API.Topic              as Topic
import           Empire.API.Response           (Response (..))
import qualified Empire.API.Response           as Response
import qualified Empire.API.Request            as Request
import           Empire.API.Request            (Request (..))
import qualified Empire.API.Topic              as T
import           Empire.Env                   (Env)
import qualified Empire.Env                   as Env
import qualified Empire.Server.Graph           as Graph
import           Empire.Server.Server          (sendToBus')
import qualified ZMQ.Bus.Bus                       as Bus
import qualified ZMQ.Bus.Data.Flag                 as Flag
import qualified ZMQ.Bus.Data.Message              as Message
import           ZMQ.Bus.Data.MessageFrame         (MessageFrame (MessageFrame))
import           ZMQ.Bus.EndPoint                  (BusEndPoints)
import qualified ZMQ.Bus.Trans                     as Bus
import           ZMQ.Bus.RPC.RPC                   as RPC

-- data Action = AddNode.Request | RemoveNodes.Request deriving (Show)

data UndoMessage = UndoMessage { _undoAction :: RemoveNodes.Request --undo.node.add/redo.node.add
                               , _redoAction :: AddNode.Request
                               } deriving (Show)

data UndoList = UndoList { _undo :: [UndoMessage]
                         , _redo :: [UndoMessage]
                         } deriving (Show)

newtype UndoT t a = UndoT (StateT UndoList t a) deriving (Applicative, Functor, Monad, MonadTrans, MonadState UndoList, MonadIO)
-- instance MonadTrans UndoT where
--     lift = UndoT
-- type Undo a = forall m. (MonadIO m, MonadState UndoList m) =>  m a

makeLenses ''UndoList
makeLenses ''UndoMessage

type Handler = ByteString -> UndoT IO ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [ makeHandler handleAddNodeUndo
    -- , makeHandler handleAddSubgraph
    -- , makeHandler handleRemoveNodes
    ]


makeHandler :: forall a. (Topic.MessageTopic a, Binary a) => (a -> UndoT IO ()) -> (String, Handler)
makeHandler h = (Topic.topic (undefined :: a), process) where
   process content = h request where request = decode . fromStrict $ content

handleAddNodeUndo :: AddNode.Response -> UndoT IO ()
handleAddNodeUndo (Response.Response _ (AddNode.Request location nodeType nodeMeta connectTo _) _ res) =
    case res of
        Response.Error  err -> return ()
        Response.Ok node -> do
            let nodeId  = node ^. Node.nodeId
                undoMsg = RemoveNodes.Request location [nodeId]
                redoMsg = AddNode.Request location nodeType nodeMeta connectTo $ Just (nodeId)
            undo %= (UndoMessage undoMsg redoMsg :)

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
                "empire.undo." -> f endPoints
                "empire.redo." -> g endPoints
                "empire." -> do collectedMessage topic content

f :: BusEndPoints -> UndoT IO ()
f endPoints = do
    h <- uses undo head
    redo %= (h :)
    undo %= tail
    let msg = h ^. undoAction
    void $ Bus.runBus endPoints $ sendMessage "undo" msg--(UndoMessage undoTopic dummyMsg)

g :: BusEndPoints -> UndoT IO ()
g endPoints = do
    h <- uses redo head
    undo %= (h :)
    redo %= tail
    let msg = h ^. redoAction
    void $ Bus.runBus endPoints $ sendMessage "redo" msg--(UndoMessage undoTopic dummyMsg)

collectedMessage :: String -> ByteString -> UndoT IO ()
collectedMessage topic content = do
    let handler   = Map.findWithDefault doNothing topic handlersMap
        doNothing _ = return ()
    void $ handler content

sendMessage :: Binary a => String -> a -> Bus.Bus ()
sendMessage topic bin = do
    void $ Bus.send Flag.Enable $ Message.Message topic $ toStrict $ Bin.encode bin

-- sendMessage :: UndoMessage -> Bus.Bus ()
-- sendMessage (UndoMessage topic msg) = do
--         void $ Bus.send Flag.Enable $ Message.Message topic msg
-- sendMessage _ = return ()
