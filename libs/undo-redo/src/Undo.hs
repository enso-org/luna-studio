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

newtype Undo a = Undo {runUndo :: StateT UndoState (Bus.BusT) a}
    deriving (Applicative, Functor, Monad, MonadState UndoState, MonadIO, MonadThrow)



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

handleMessage :: Message.Message -> Undo ()
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


type Handler z = ByteString -> Undo ()

-- FIXME[WD]: robisz newtype nad statem i mozliwosc rejestracji tych rzeczy z kazdewgo pliku
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

-- FIXME[WD]: rozdielenie odpowiedzialnosci - poszczegolne implementacje do odpowiednich plikow
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
--FIXME
-- type family RedoResponseRequest t
-- type instance RedoResponseRequest AddNode.Response              = AddNode.Request
-- type instance RedoResponseRequest AddSubgraph.Response          = AddSubgraph.Request
-- type instance RedoResponseRequest RemoveNodes.Response          = RemoveNodes.Request
-- type instance RedoResponseRequest UpdateNodeExpression.Response = UpdateNodeExpression.Request
-- type instance RedoResponseRequest UpdateNodeMeta.Response       = UpdateNodeMeta.Request
-- type instance RedoResponseRequest RenameNode.Response           = RenameNode.Request
-- type instance RedoResponseRequest Connect.Response              = Connect.Request
-- type instance RedoResponseRequest Disconnect.Response           = Disconnect.Request

-- FIXME[WD]: nie uzywamy liftIO bo powinno byc juz zliftowane zaqwsze, jak nie jest to w biblitoece zl iftowac
flushHelper :: MonadIO m => String -> m ()
flushHelper text = liftIO $ putStrLn text >> hFlush stdout

--FIXME[WD]: Response.Response -> Response + jak sien ie da lub jakies dziwne -> phabricator dolacz wd
--FIXME[WD]: String -> ?
--FIXME[WD]: Sprobujmy nie uzywac tuplui

data BusErrorException = BusErrorException deriving (Show)
instance Exception BusErrorException

makeHandler :: forall req inv res z. (Topic.MessageTopic (Response.Response req inv res), Binary (Response.Response req inv res),
            Topic.MessageTopic (Request.Request (UndoResponseRequest (Response.Response req inv res))), Binary (UndoResponseRequest (Response.Response req inv res)),
            Topic.MessageTopic (Request.Request (RedoResponseRequest (Response.Response req inv res))), Binary (RedoResponseRequest (Response.Response req inv res)))
            => (Response.Response req inv res -> Maybe (UndoRequests (Response.Response req inv res))) -> (String, Handler z)
makeHandler h =
    let process content = let response   = decode . fromStrict $ content
                              maybeGuiID = response ^. Response.guiID
                              guiID      = fromJust maybeGuiID --FIXME zrób case albo coś innego
                              reqUUID    = response ^. Response.requestId
                          in case h response of
                              Nothing     -> throwM BusErrorException
                              Just (r, q) -> handle guiID reqUUID (Topic.topic (Request.Request UUID.nil Nothing r)) r (Topic.topic (Request.Request UUID.nil Nothing q)) q
    in (Topic.topic (undefined :: Response.Response req inv res), process)
    -- FIXME[WD]: nie uzywamy undefined, nigdy

handle :: (Binary a, Binary b) => GuiID -> ReqUUID -> Topic.Topic -> a -> Topic.Topic -> b -> Undo ()
handle guiID reqUUID topicUndo undoReq topicRedo redoReq = do
    undo    %= (UndoMessage guiID reqUUID topicUndo undoReq topicRedo redoReq :)
    redo    .= []
    history %= (UndoMessage guiID reqUUID topicUndo undoReq topicRedo redoReq :)

withOk :: Response.Status a -> (a -> Maybe b) -> Maybe b
withOk (Response.Error _) _ = Nothing
withOk (Response.Ok a)    f = f a

handleAddNodeUndo :: AddNode.Response -> Maybe (RemoveNodes.Request, AddNode.Request)
handleAddNodeUndo (Response.Response _ _ (AddNode.Request location nodeType nodeMeta connectTo _) _ res) =
    withOk res $ \node ->
        let nodeId  = node ^. Node.nodeId
            undoMsg = RemoveNodes.Request location [nodeId]
            redoMsg = AddNode.Request location nodeType nodeMeta connectTo $ Just (nodeId)
        in Just (undoMsg, redoMsg)

handleAddSubgraphUndo :: AddSubgraph.Response -> Maybe (RemoveNodes.Request, AddSubgraph.Request)
handleAddSubgraphUndo (Response.Response _ _ (AddSubgraph.Request location nodes connections saveNodeIds) _ res ) =
    withOk res $ \idMapping ->
        case idMapping of
            Nothing -> let ids     = map (^. Node.nodeId) nodes
                           undoMsg = RemoveNodes.Request location ids
                           redoMsg = AddSubgraph.Request location nodes connections True
                       in Just (undoMsg, redoMsg)
            Just idsMap -> let nodes'       = flip map nodes $ Node.nodeId %~ (idsMap Map.!)
                               connections' = map (\conn -> conn & Connection.src . PortRef.srcNodeId %~ (idsMap Map.!)
                                                                 & Connection.dst . PortRef.dstNodeId %~ (idsMap Map.!)
                                                                 ) connections
                               ids'    = map (^. Node.nodeId) nodes'
                               undoMsg = RemoveNodes.Request location ids'
                               redoMsg = AddSubgraph.Request location nodes' connections' True
                           in Just (undoMsg, redoMsg)

connect :: (OutPortRef, InPortRef) -> Connection
connect = uncurry Connection

filterNodes :: [NodeId] -> [Node] -> [Node]
filterNodes nodesIds nodes = catMaybes p
    where q nId = List.find (\node -> node ^. Node.nodeId == nId) nodes
          p   = map q nodesIds

filterConnections :: [(OutPortRef, InPortRef)] -> [NodeId] -> [(OutPortRef, InPortRef)]
filterConnections connectionPorts nodesIds = concat p
    where  q nId = Prologue.filter (\port -> (((PortRef.OutPortRef' (fst port)) ^. PortRef.nodeId == nId) || ( (PortRef.InPortRef' (snd port)) ^. PortRef.nodeId == nId)) ) connectionPorts
           p   = map q nodesIds

handleRemoveNodesUndo :: RemoveNodes.Response -> Maybe (AddSubgraph.Request, RemoveNodes.Request)
handleRemoveNodesUndo (Response.Response _ _ (RemoveNodes.Request location nodesIds) inv _) =
    withOk inv $ \(RemoveNodes.Inverse nodes connectionPorts) ->
        let deletedNodes        = filterNodes nodesIds nodes
            deletedConnections  = filterConnections connectionPorts nodesIds
            connections         = connect <$> deletedConnections
            undoMsg             = AddSubgraph.Request location deletedNodes connections True -- FIXME [WD]: nie uzywamy literalow, nigdy
            redoMsg             = RemoveNodes.Request location nodesIds
        in Just (undoMsg, redoMsg)

handleUpdateNodeExpressionUndo :: UpdateNodeExpression.Response -> Maybe ( UpdateNodeExpression.Request,  UpdateNodeExpression.Request)
handleUpdateNodeExpressionUndo (Response.Response _ _ (UpdateNodeExpression.Request location nodeId expression) inv res) =
    withOk inv $ \(UpdateNodeExpression.Inverse oldExpr) ->
        let undoMsg = UpdateNodeExpression.Request location nodeId oldExpr
            redoMsg = UpdateNodeExpression.Request location nodeId expression
        in Just (undoMsg, redoMsg)

tupleUpdate :: Node -> SingleUpdate
tupleUpdate node = (node ^. Node.nodeId, node ^. Node.nodeMeta)

filterMeta :: [Node] -> [SingleUpdate] -> [SingleUpdate]
filterMeta allNodes updates = map tupleUpdate p
    where  nodeIds = map fst updates
           q x     = List.find (\node -> node ^. Node.nodeId == x) allNodes
           p       = catMaybes $ map q nodeIds

handleUpdateNodeMetaUndo :: UpdateNodeMeta.Response -> Maybe (UpdateNodeMeta.Request, UpdateNodeMeta.Request)
handleUpdateNodeMetaUndo (Response.Response _ _ (UpdateNodeMeta.Request location updates) inv _) =
    withOk inv $ \(UpdateNodeMeta.Inverse nodes) ->
    let prevMeta = filterMeta nodes updates
        undoMsg  = UpdateNodeMeta.Request location prevMeta
        redoMsg  = UpdateNodeMeta.Request location updates
    in Just (undoMsg, redoMsg)

handleRenameNodeUndo :: RenameNode.Response ->  Maybe (RenameNode.Request, RenameNode.Request)
handleRenameNodeUndo (Response.Response _ _ (RenameNode.Request location nodeId name) inv res) =
    withOk inv $ \(RenameNode.Inverse namePrev) -> Just $ case namePrev of
        Nothing -> let undoMsg = RenameNode.Request location nodeId "" -- FIXME [WD]: nie uzywamy literalow, nigdy - jak to node moze nie miec nazwy
                       redoMsg = RenameNode.Request location nodeId name
                   in (undoMsg, redoMsg)
        Just nameOld -> let undoMsg = RenameNode.Request location nodeId nameOld
                            redoMsg = RenameNode.Request location nodeId name
                        in (undoMsg, redoMsg)

-- FIXME[WD]: hlint - na calym pliku
handleConnectUndo :: Connect.Response -> Maybe (Disconnect.Request, Connect.Request)
handleConnectUndo (Response.Response _ _ (Connect.Request location src dst) inv res) =
    withOk res . const $
        let undoMsg = Disconnect.Request location dst
            redoMsg = Connect.Request location src dst
        in Just (undoMsg, redoMsg)

handleDisconnectUndo :: Disconnect.Response -> Maybe (Connect.Request, Disconnect.Request)
handleDisconnectUndo (Response.Response _ _ (Disconnect.Request location dst) inv res) =
    withOk inv $ \(Disconnect.Inverse src) ->
        let undoMsg = Connect.Request location src dst
            redoMsg = Disconnect.Request location dst
        in Just (undoMsg, redoMsg)
