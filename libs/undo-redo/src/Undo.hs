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
import qualified Data.Text.Lazy                        as Text

import qualified Data.Map.Strict                   as Map
import           Data.Map.Strict                   (Map)
import           Data.Maybe
import           Data.Set                          hiding (map)
import           Prologue                          hiding (throwM)

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
import qualified Empire.API.Graph.RemoveNodes      as RemoveNodes
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.UpdateNodeExpression as UpdateNodeExpression
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
import           ZMQ.Bus.RPC.RPC                   as RPC

import System.IO (stdout,hFlush)




data UndoMessage where
    UndoMessage :: (Binary undoReq, Binary redoReq) => Topic.Topic -> undoReq -> Topic.Topic -> redoReq -> UndoMessage

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
    , makeHandler handleConnectUndo
    , makeHandler handleDisconnectUndo
    ]

type UndoRequests a = (UndoResponseRequest a, RedoResponseRequest a)

type family UndoResponseRequest t where
    UndoResponseRequest AddNode.Response              = RemoveNodes.Request
    UndoResponseRequest AddSubgraph.Response          = RemoveNodes.Request
    UndoResponseRequest RemoveNodes.Response          = AddSubgraph.Request
    UndoResponseRequest UpdateNodeExpression.Response = UpdateNodeExpression.Request
    UndoResponseRequest RenameNode.Response           = RenameNode.Request
    UndoResponseRequest Connect.Response              = Disconnect.Request
    UndoResponseRequest Disconnect.Response           = Connect.Request

type family RedoResponseRequest t where
    RedoResponseRequest AddNode.Response              = AddNode.Request
    RedoResponseRequest AddSubgraph.Response          = AddSubgraph.Request
    RedoResponseRequest RemoveNodes.Response          = RemoveNodes.Request
    RedoResponseRequest UpdateNodeExpression.Response = UpdateNodeExpression.Request
    RedoResponseRequest RenameNode.Response           = RenameNode.Request
    RedoResponseRequest Connect.Response              = Connect.Request
    RedoResponseRequest Disconnect.Response           = Disconnect.Request

flushHelper :: MonadIO m => String -> m ()
flushHelper text = liftIO $ putStrLn text >> hFlush stdout

makeHandler :: forall response. (Topic.MessageTopic response, Binary response,
            Topic.MessageTopic (Request.Request (UndoResponseRequest response)), Binary (UndoResponseRequest response),
            Topic.MessageTopic (Request.Request (RedoResponseRequest response)), Binary (RedoResponseRequest response))
            => (response -> Maybe (UndoRequests response)) -> (String, Handler)
makeHandler h =
    let process content = let request = decode . fromStrict $ content
                          in case h request of
                              Nothing           -> throwM BusErrorException
                              Just (r, q) -> handle  (Topic.topic (Request.Request UUID.nil r)) r (Topic.topic (Request.Request UUID.nil q)) q
    in (Topic.topic (undefined :: response), process)

handle :: (Binary a, Binary b) => Topic.Topic -> a -> Topic.Topic -> b -> UndoT IO ()
handle topicUndo undoReq topicRedo redoReq = undo %= (UndoMessage topicUndo undoReq topicRedo redoReq :)

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

connect :: (OutPortRef, InPortRef) -> Connection
connect ports = Connection src dst
    where src = fst ports
          dst = snd ports

handleRemoveNodesUndo :: RemoveNodes.Response -> Maybe (AddSubgraph.Request, RemoveNodes.Request)
handleRemoveNodesUndo (Response.Response _ (RemoveNodes.Request location noddesIds) inv _) =
    withOk inv $ \(RemoveNodes.Inverse nodes connectionPorts)->
        let connections = connect <$> connectionPorts
            undoMsg = AddSubgraph.Request location nodes connections
            redoMsg = RemoveNodes.Request location noddesIds
        in Just (undoMsg, redoMsg)

handleUpdateNodeExpressionUndo :: UpdateNodeExpression.Response -> Maybe ( UpdateNodeExpression.Request,  UpdateNodeExpression.Request)
handleUpdateNodeExpressionUndo (Response.Response _ (UpdateNodeExpression.Request location nodeId expression) inv res) =
    withOk inv $ \(UpdateNodeExpression.Inverse oldExpr) ->
        let undoMsg = UpdateNodeExpression.Request location nodeId oldExpr
            redoMsg = UpdateNodeExpression.Request location nodeId expression
        in Just (undoMsg, redoMsg)

handleRenameNodeUndo :: RenameNode.Response ->  Maybe (RenameNode.Request, RenameNode.Request)
handleRenameNodeUndo (Response.Response _ (RenameNode.Request location nodeId name) inv res) =
    withOk inv $ \(RenameNode.Inverse namePrev) ->
        case namePrev of
            Nothing -> let undoMsg = RenameNode.Request location nodeId ""
                           redoMsg = RenameNode.Request location nodeId name
                       in Just (undoMsg, redoMsg)
            Just nameOld -> let undoMsg = RenameNode.Request location nodeId nameOld
                                redoMsg = RenameNode.Request location nodeId name
                            in Just (undoMsg, redoMsg)

handleConnectUndo :: Connect.Response -> Maybe (Disconnect.Request, Connect.Request)
handleConnectUndo (Response.Response _ (Connect.Request location src dst) inv res) =
    withOk res $ const $
        let undoMsg = Disconnect.Request location dst
            redoMsg = Connect.Request location src dst
        in Just (undoMsg, redoMsg)

handleDisconnectUndo :: Disconnect.Response -> Maybe (Connect.Request, Disconnect.Request)
handleDisconnectUndo (Response.Response _ (Disconnect.Request location dst) inv res) =
    withOk inv $ \(Disconnect.Inverse src) ->
        let undoMsg = Connect.Request location src dst
            redoMsg = Disconnect.Request location dst
        in Just (undoMsg, redoMsg)

empty :: History
empty = History [] []

runUndo :: BusEndPoints -> History -> IO ()
runUndo endPoints state = do
    flushHelper $ show $ Map.keys handlersMap
    let UndoT collect = do
            msg <- receiveEvent endPoints
            flushHelper $ show $ msg
            collectEvents msg
    runReaderT (evalStateT (forever collect) state) endPoints

data BusErrorException = BusErrorException deriving (Show)
instance Exception BusErrorException

receiveEvent :: (MonadThrow m, MonadIO m) => BusEndPoints -> m MessageFrame --fixme [SB] przenies runBus do runUndo
receiveEvent endPoints = do
    msgFrame <- Bus.runBus endPoints $ do
         Bus.subscribe "empire."
         Bus.receive
    case msgFrame of
        Left err  -> throwM BusErrorException
        Right msg -> return msg

collectEvents :: MessageFrame -> UndoT IO ()
collectEvents (MessageFrame msg corId senderId lastFrm) = do
    let topic = msg ^. Message.topic
        userId = show senderId
        content = msg ^. Message.message
    flushHelper $ show $ topic
    case topic of
        "empire.undo.request" -> do
            req <- doUndo
            flushHelper "runUndo"
            takeEndPointsAndRun $ sendUndo req
        "empire.redo.request" -> do
            req <- doRedo
            takeEndPointsAndRun $ sendRedo req
        _ -> do collectedMessage topic content

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
sendUndo msg = sendMessage ActUndo msg

sendRedo :: UndoMessage -> Bus.Bus ()
sendRedo msg = sendMessage ActRedo msg

sendMessage :: Action -> UndoMessage -> Bus.Bus ()
sendMessage action msg = do
    uuid <- liftIO $ UUID.nextRandom
    void $ Bus.send Flag.Enable $ case action of
        ActUndo -> case msg of UndoMessage tu u _ _ -> Message.Message tu $ toStrict $ Bin.encode $ Request.Request uuid u
        ActRedo -> case msg of UndoMessage _ _ tr r -> Message.Message tr $ toStrict $ Bin.encode $ Request.Request uuid r
