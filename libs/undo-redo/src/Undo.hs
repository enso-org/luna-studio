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
    [ makeHandler handleAddSubgraphUndo
    , makeHandler handleAddNodeUndo
    , makeHandler handleRemoveNodesUndo
    , makeHandler handleUpdateNodeExpressionUndo
    , makeHandler handleRenameNodeUndo
    ]

type UndoRequests a = (UndoRequest a, RedoRequest a)

type family UndoRequest t where
    UndoRequest AddNode.Response              = RemoveNodes.Request
    UndoRequest AddSubgraph.Response          = RemoveNodes.Request
    UndoRequest RemoveNodes.Response          = AddSubgraph.Request
    UndoRequest UpdateNodeExpression.Response = UpdateNodeExpression.Request
    UndoRequest RenameNode.Response           = RenameNode.Request

type family RedoRequest t where
    RedoRequest AddNode.Response              = AddNode.Request
    RedoRequest AddSubgraph.Response          = AddSubgraph.Request
    RedoRequest RemoveNodes.Response          = RemoveNodes.Request
    RedoRequest UpdateNodeExpression.Response = UpdateNodeExpression.Request
    RedoRequest RenameNode.Response           = RenameNode.Request


makeHandler :: forall response. (Topic.MessageTopic response, Binary response, Binary (UndoRequest response), Binary (RedoRequest response))
            => (response -> Maybe (UndoRequests response)) -> (String, Handler)
makeHandler h =
    let process content = let request = decode . fromStrict $ content
                          in case h request of
                              Nothing           -> throwM BusErrorException
                              Just (r, q) -> handle r q
    in (Topic.topic (undefined :: response), process)

handle :: (Binary a, Binary b) => a -> b -> UndoT IO ()
handle undoReq redoReq = undo %= (UndoMessage undoReq redoReq :)

withOk :: Response.Status a -> (a -> Maybe b) -> Maybe b
withOk (Response.Error _) _ = Nothing
withOk (Response.Ok a)    f = f a

handleAddNodeUndo :: AddNode.Response -> Maybe (RemoveNodes.Request, AddNode.Request)
handleAddNodeUndo (Response.Response _ (AddNode.Request location nodeType nodeMeta connectTo _) _ res) =
    withOk res $ \node ->
        let nodeId  = node ^. Node.nodeId
            undoMsg = RemoveNodes.Request location [nodeId]
            redoMsg = AddNode.Request location nodeType nodeMeta connectTo $ Just (nodeId)
        in Just (undoMsg, redoMsg)

handleAddSubgraphUndo :: AddSubgraph.Response -> Maybe (RemoveNodes.Request, AddSubgraph.Request)  --dodac result do pola addsubgraph response bo zwraca request z podmienionym id
handleAddSubgraphUndo (Response.Response  _ (AddSubgraph.Request location nodes connections) _ res ) =
    withOk res $ const $
        let ids = map (^. Node.nodeId) nodes
            undoMsg = RemoveNodes.Request location ids
            redoMsg = AddSubgraph.Request location nodes connections
        in Just (undoMsg, redoMsg)


handleRemoveNodesUndo :: RemoveNodes.Response -> Maybe (AddSubgraph.Request, RemoveNodes.Request)
handleRemoveNodesUndo (Response.Response _ (RemoveNodes.Request location noddesIds) inv res) =
    withOk inv $ \(RemoveNodes.Inverse nodes connections)->
        let undoMsg = AddSubgraph.Request location nodes connections
            redoMsg = RemoveNodes.Request location noddesIds
        in Just (undoMsg, redoMsg)

handleUpdateNodeExpressionUndo :: UpdateNodeExpression.Response -> Maybe ( UpdateNodeExpression.Request,  UpdateNodeExpression.Request)
handleUpdateNodeExpressionUndo (Response.Response _ (UpdateNodeExpression.Request location nodeId expression) oldExpr res) =
    withOk res $ const $
        let undoMsg = UpdateNodeExpression.Request location nodeId oldExpr
            redoMsg = UpdateNodeExpression.Request location nodeId expression
        in Just (undoMsg, redoMsg)

handleRenameNodeUndo :: RenameNode.Response -> Maybe (RenameNode.Request, RenameNode.Request)
handleRenameNodeUndo (Response.Response _ (RenameNode.Request location nodeId name) namePrev res) =
    withOk res $ const $
        let undoMsg = RenameNode.Request location nodeId namePrev
            redoMsg = RenameNode.Request location nodeId name
        in Just (undoMsg, redoMsg)

-- handleConnectUndo

-- handleDisconnectUndo :: Disconnect.Response -> Maybe (Connect.Request, Disconnect.Request)
-- handleDisconnectUndo (Response.Response _ (Disconnect.Request location dst) connections res)
--     withOk res $ const $
--         let undoMsg = Connect.Request location

empty :: History
empty = History [] []

runUndo :: BusEndPoints -> History -> IO ()
runUndo endPoints state = do
    let UndoT collect = do
            msg <- receiveEvent endPoints
            collectEvents msg
    runReaderT (evalStateT collect state) endPoints

data BusErrorException = BusErrorException deriving (Show)
instance Exception BusErrorException

receiveEvent :: (MonadThrow m, MonadIO m) => BusEndPoints -> m MessageFrame
receiveEvent endPoints = do
    msgFrame <- Bus.runBus endPoints $ Bus.receive
    case msgFrame of
        Left err  -> throwM BusErrorException
        Right msg -> return msg

collectEvents :: MessageFrame -> UndoT IO ()
collectEvents (MessageFrame msg corId senderId lastFrm) = do
    let topic = msg ^. Message.topic
        userId = show senderId
        content = msg ^. Message.message
    case topic of
        "empire.undo" -> do
            req <- doUndo
            takeEndPointsAndRun $ sendUndo req
        "empire.redo" -> do
            req <- doRedo
            takeEndPointsAndRun $ sendRedo req
        "empire." -> do collectedMessage topic content

doUndo :: UndoT IO UndoMessage
doUndo = do
    h <- uses undo head
    redo %= (h :)
    undo %= tail
    return h

doRedo :: UndoT IO UndoMessage
doRedo = do
    h <- uses redo head
    undo %= (h :)
    redo %= tail
    return h

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
