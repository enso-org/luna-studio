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
import qualified Data.List as List
import qualified Data.Map.Strict                   as Map
import           Data.Map.Strict                   (Map)
import           Data.Maybe
import           Data.Set                          hiding (map)
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
import           ZMQ.Bus.RPC.RPC                   as RPC
import qualified ZMQ.Bus.Data.Topic              as ZMQTopic
import qualified ZMQ.Bus.Env                     as Env
import           System.ZMQ4.Monadic             (ZMQ)
import qualified System.ZMQ4.Monadic             as ZMQ
import qualified ZMQ.Bus.EndPoint                as EP
import qualified ZMQ.Bus.Data.MessageFrame       as MessageFrame
import qualified ZMQ.RPC.Client                  as Client
import qualified ZMQ.Bus.Control.Handler.Methods as Methods
import           Control.Error                   (ExceptT, hoistEither, runExceptT)

import System.IO (stdout,hFlush)




data UndoMessage where
    UndoMessage :: (Binary undoReq, Binary redoReq) => UUID -> Topic.Topic -> undoReq -> Topic.Topic -> redoReq -> UndoMessage

data UndoState z = UndoState { _undo :: [UndoMessage]
                           , _redo :: [UndoMessage]
                           , _bus :: Env.BusEnv z
                           }
makeLenses ''UndoState

newtype Undo z a = Undo (StateT (UndoState z) (ReaderT BusEndPoints (ZMQ z)) a)
    deriving (Applicative, Functor, Monad, MonadState (UndoState z), MonadReader BusEndPoints, MonadIO, MonadThrow)



data Action = ActUndo | ActRedo

type Handler z = ByteString -> Undo z ()

handlersMap :: Map String (Handler z)
handlersMap = Map.fromList
    [ makeHandler handleAddSubgraphUndo
    , makeHandler handleAddNodeUndo
    , makeHandler handleRemoveNodesUndo
    , makeHandler handleUpdateNodeExpressionUndo
    , makeHandler handleUpdateNodeMetaUndo
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
    UndoResponseRequest UpdateNodeMeta.Response       = UpdateNodeMeta.Request
    UndoResponseRequest RenameNode.Response           = RenameNode.Request
    UndoResponseRequest Connect.Response              = Disconnect.Request
    UndoResponseRequest Disconnect.Response           = Connect.Request

type family RedoResponseRequest t where
    RedoResponseRequest AddNode.Response              = AddNode.Request
    RedoResponseRequest AddSubgraph.Response          = AddSubgraph.Request
    RedoResponseRequest RemoveNodes.Response          = RemoveNodes.Request
    RedoResponseRequest UpdateNodeExpression.Response = UpdateNodeExpression.Request
    RedoResponseRequest UpdateNodeMeta.Response       = UpdateNodeMeta.Request
    RedoResponseRequest RenameNode.Response           = RenameNode.Request
    RedoResponseRequest Connect.Response              = Connect.Request
    RedoResponseRequest Disconnect.Response           = Disconnect.Request

flushHelper :: MonadIO m => String -> m ()
flushHelper text = liftIO $ putStrLn text >> hFlush stdout

makeHandler :: forall t u v z. (Topic.MessageTopic (Response.Response t u v), Binary (Response.Response t u v),
            Topic.MessageTopic (Request.Request (UndoResponseRequest (Response.Response t u v))), Binary (UndoResponseRequest (Response.Response t u v)),
            Topic.MessageTopic (Request.Request (RedoResponseRequest (Response.Response t u v))), Binary (RedoResponseRequest (Response.Response t u v)))
            => ((Response.Response t u v) -> Maybe (UndoRequests (Response.Response t u v))) -> (String, (Handler z))
makeHandler h =
    let process content = let response = decode . fromStrict $ content
                              maybeGuiID = response ^. Response.guiID
                              guiID = fromJust maybeGuiID --fixme zrób case albo coś innego
                          in case h response of
                              Nothing     -> throwM BusErrorException
                              Just (r, q) -> handle guiID (Topic.topic (Request.Request UUID.nil Nothing r)) r (Topic.topic (Request.Request UUID.nil Nothing q)) q
    in (Topic.topic (undefined :: (Response.Response t u v)), process)

handle :: (Binary a, Binary b) => UUID -> Topic.Topic -> a -> Topic.Topic -> b -> Undo z ()
handle guiID topicUndo undoReq topicRedo redoReq = undo %= (UndoMessage guiID topicUndo undoReq topicRedo redoReq :)

withOk :: Response.Status a -> (a -> Maybe b) -> Maybe b
withOk (Response.Error _) _ = Nothing
withOk (Response.Ok a)    f = f a

handleAddNodeUndo :: AddNode.Response -> Maybe (RemoveNodes.Request, AddNode.Request)
handleAddNodeUndo (Response.Response _  guiID (AddNode.Request location nodeType nodeMeta connectTo _) _ res) =
    withOk res $ \node ->
        let nodeId  = node ^. Node.nodeId
            undoMsg = RemoveNodes.Request location [nodeId]
            redoMsg = AddNode.Request location nodeType nodeMeta connectTo $ Just (nodeId)
        in Just (undoMsg, redoMsg)

handleAddSubgraphUndo :: AddSubgraph.Response -> Maybe (RemoveNodes.Request, AddSubgraph.Request)  --dodac result do pola addsubgraph response bo zwraca request z podmienionym id
handleAddSubgraphUndo (Response.Response  _ guiID (AddSubgraph.Request location nodes connections saveNodeIds) _ res ) =
    withOk res $ \idMapping ->
        case idMapping of
            Nothing -> let ids = map (^. Node.nodeId) nodes
                           undoMsg = RemoveNodes.Request location ids
                           redoMsg = AddSubgraph.Request location nodes connections True
                       in Just (undoMsg, redoMsg)
            Just idsMap -> let nodes' = flip map nodes $ Node.nodeId %~ (idsMap Map.!)
                               connections' = map (\conn -> conn & Connection.src . PortRef.srcNodeId %~ (idsMap Map.!)
                                                                 & Connection.dst . PortRef.dstNodeId %~ (idsMap Map.!)
                                                                 ) connections
                               ids' = map (^. Node.nodeId) nodes'
                               undoMsg = RemoveNodes.Request location ids'
                               redoMsg = AddSubgraph.Request location nodes' connections' True
                           in Just (undoMsg, redoMsg)

connect :: (OutPortRef, InPortRef) -> Connection
connect ports = Connection src dst
    where src = fst ports
          dst = snd ports

filterNodes :: [NodeId] -> [Node] -> [Node]
filterNodes nodesIds nodes =
    let q x = List.find (\node -> node ^. Node.nodeId == x) nodes
        p = map q nodesIds
    in catMaybes p

filterConnections :: [(OutPortRef, InPortRef)] -> [NodeId] -> [(OutPortRef, InPortRef)]
filterConnections connectionPorts nodesIds =
    let q x = Prologue.filter (\port -> (((PortRef.OutPortRef' (fst port)) ^. PortRef.nodeId == x) || ( (PortRef.InPortRef' (snd port)) ^. PortRef.nodeId == x)) ) connectionPorts
        p = map q nodesIds
    in concat p

handleRemoveNodesUndo :: RemoveNodes.Response -> Maybe (AddSubgraph.Request, RemoveNodes.Request)
handleRemoveNodesUndo (Response.Response _ guiID (RemoveNodes.Request location nodesIds) inv _) =
    withOk inv $ \(RemoveNodes.Inverse nodes connectionPorts) ->
        let deletedNodes = filterNodes nodesIds nodes
            deletedConnections  = filterConnections connectionPorts nodesIds
            connections = connect <$> deletedConnections
            undoMsg = AddSubgraph.Request location deletedNodes connections True
            redoMsg = RemoveNodes.Request location nodesIds
        in Just (undoMsg, redoMsg)

handleUpdateNodeExpressionUndo :: UpdateNodeExpression.Response -> Maybe ( UpdateNodeExpression.Request,  UpdateNodeExpression.Request)
handleUpdateNodeExpressionUndo (Response.Response _ guiID (UpdateNodeExpression.Request location nodeId expression) inv res) =
    withOk inv $ \(UpdateNodeExpression.Inverse oldExpr) ->
        let undoMsg = UpdateNodeExpression.Request location nodeId oldExpr
            redoMsg = UpdateNodeExpression.Request location nodeId expression
        in Just (undoMsg, redoMsg)

tupleUpdate :: Node -> SingleUpdate
tupleUpdate node = (node ^. Node.nodeId, node ^. Node.nodeMeta)

filterMeta :: [Node] -> [SingleUpdate] -> [SingleUpdate]
filterMeta allNodes updates =
    let nodeIds = map fst updates
        q x  = List.find (\node -> node ^. Node.nodeId == x) allNodes
        p = catMaybes $ map q nodeIds
    in map tupleUpdate p

handleUpdateNodeMetaUndo :: UpdateNodeMeta.Response -> Maybe (UpdateNodeMeta.Request, UpdateNodeMeta.Request)
handleUpdateNodeMetaUndo (Response.Response _ guiID (UpdateNodeMeta.Request location updates) inv _) =
    withOk inv $ \(UpdateNodeMeta.Inverse nodes) ->
    let prevMeta = filterMeta nodes updates
        undoMsg = UpdateNodeMeta.Request location prevMeta
        redoMsg = UpdateNodeMeta.Request location updates
    in Just (undoMsg, redoMsg)

handleRenameNodeUndo :: RenameNode.Response ->  Maybe (RenameNode.Request, RenameNode.Request)
handleRenameNodeUndo (Response.Response _ guiID (RenameNode.Request location nodeId name) inv res) =
    withOk inv $ \(RenameNode.Inverse namePrev) ->
        case namePrev of
            Nothing -> let undoMsg = RenameNode.Request location nodeId ""
                           redoMsg = RenameNode.Request location nodeId name
                       in Just (undoMsg, redoMsg)
            Just nameOld -> let undoMsg = RenameNode.Request location nodeId nameOld
                                redoMsg = RenameNode.Request location nodeId name
                            in Just (undoMsg, redoMsg)

handleConnectUndo :: Connect.Response -> Maybe (Disconnect.Request, Connect.Request)
handleConnectUndo (Response.Response _ guiID (Connect.Request location src dst) inv res) =
    withOk res $ const $
        let undoMsg = Disconnect.Request location dst
            redoMsg = Connect.Request location src dst
        in Just (undoMsg, redoMsg)

handleDisconnectUndo :: Disconnect.Response -> Maybe (Connect.Request, Disconnect.Request)
handleDisconnectUndo (Response.Response _ guiID (Disconnect.Request location dst) inv res) =
    withOk inv $ \(Disconnect.Inverse src) ->
        let undoMsg = Connect.Request location src dst
            redoMsg = Disconnect.Request location dst
        in Just (undoMsg, redoMsg)


runUndo :: BusEndPoints -> IO ()
runUndo endPoints = ZMQ.runZMQ $ do
    clientID <- do
        socket <- ZMQ.socket ZMQ.Req
        ZMQ.connect socket $ EP.controlEndPoint endPoints
        let request = Methods.CreateID
        Right response <- runExceptT $ Client.query socket request
        ZMQ.close socket
        return $ Methods.clientID response
    subSocket  <- ZMQ.socket ZMQ.Sub
    ZMQ.subscribe subSocket $ ZMQTopic.toByteString "empire."
    pushSocket <- ZMQ.socket ZMQ.Push
    ZMQ.connect subSocket  $ EP.pubEndPoint  endPoints
    ZMQ.connect pushSocket $ EP.pullEndPoint endPoints

    let Undo collect = do
            forever $ do
                msg <- receiveEvent subSocket
                collectEvents msg clientID pushSocket
        state = UndoState [] [] (Env.BusEnv subSocket pushSocket clientID 0)
    flip runReaderT endPoints $ evalStateT collect state


data BusErrorException = BusErrorException deriving (Show)
instance Exception BusErrorException

receive :: ZMQ.Socket z ZMQ.Sub -> Undo z ByteString
receive sub = Undo $ lift $ lift $ ZMQ.receive sub

receiveEvent :: ZMQ.Socket z ZMQ.Sub -> Undo z MessageFrame --fixme [SB] przenies runBus do runUndo
receiveEvent sub = do
    msg <- receive sub
    let res = MessageFrame.fromByteString msg
    case res of
        Left _ -> throwM BusErrorException
        Right m -> return m

collectEvents :: MessageFrame -> Message.ClientID -> ZMQ.Socket z ZMQ.Push -> Undo z ()
collectEvents (MessageFrame msg corId senderId lastFrm) myId push = do
    let topic   = msg ^. Message.topic
        content = msg ^. Message.message
    flushHelper $ show $ topic
    flushHelper "myId: "
    flushHelper $ show $ myId
    flushHelper "clientId: "
    flushHelper $ show $ senderId

    case topic of
        "empire.undo.request" -> do
            req <- doUndo
            flushHelper "runUndo"
            sendUndo req push myId
        "empire.redo.request" -> do
            req <- doRedo
            sendRedo req push myId
        _ -> Control.Monad.State.when (myId /= senderId) $
                do
                    collectedMessage topic content
                    flushHelper "addmsgToQueue"


doUndo :: MonadState (UndoState z) m => m UndoMessage
doUndo = do
    h <- uses undo head
    redo %= (h :)
    undo %= tail
    return h

doRedo :: MonadState (UndoState z) m => m UndoMessage
doRedo = do
    h <- uses redo head
    undo %= (h :)
    redo %= tail
    return h

collectedMessage :: String -> ByteString -> Undo z ()
collectedMessage topic content = do
    let handler   = Map.findWithDefault doNothing topic handlersMap
        doNothing _ = return ()
    void $ handler content

send :: ZMQ.Socket z ZMQ.Push -> Message.ClientID -> Message.Message -> Undo z ()
send push clientID msg = do
    requestID <- do
        s <- use bus
        let requestID = Env.requestID s
        bus .= s { Env.requestID = requestID + 1 }
        return requestID
    let correlationID = Message.CorrelationID clientID requestID
        msgFrame = MessageFrame msg correlationID clientID Flag.Enable
        bytestr = MessageFrame.toByteString msgFrame
    Undo $ lift $ lift $ ZMQ.send push [] bytestr

sendUndo :: UndoMessage -> ZMQ.Socket z ZMQ.Push -> Message.ClientID -> Undo z ()
sendUndo msg push clientID = sendMessage push clientID ActUndo msg

sendRedo :: UndoMessage -> ZMQ.Socket z ZMQ.Push -> Message.ClientID -> Undo z ()
sendRedo msg push clientID = sendMessage push clientID ActRedo msg

sendMessage :: ZMQ.Socket z ZMQ.Push -> Message.ClientID -> Action -> UndoMessage -> Undo z ()
sendMessage push clientID action msg = do
    uuid <- liftIO $ UUID.nextRandom
    void $ send push clientID $ case action of
        ActUndo -> case msg of UndoMessage _ tu u _ _ -> Message.Message tu $ toStrict $ Bin.encode $ Request.Request uuid Nothing u
        ActRedo -> case msg of UndoMessage _ _ _ tr r -> Message.Message tr $ toStrict $ Bin.encode $ Request.Request uuid Nothing r
