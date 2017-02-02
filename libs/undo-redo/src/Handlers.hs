{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers where
import UndoState

import           Control.Exception                 (Exception)
import           Control.Exception.Safe            (MonadThrow, throwM)
import           Data.ByteString                   (ByteString, empty)
import           Data.ByteString.Lazy              (toStrict,fromStrict)
import           Data.Binary                       (Binary, decode)
import qualified Data.Binary                       as Bin
import qualified Data.List as List
import qualified Data.Map.Strict                   as Map
import           Data.Map.Strict                   (Map)
import           Data.Maybe
import qualified Data.Set                          as Set
import           Prologue                          hiding (throwM)

import           Data.UUID as UUID (nil)
import           Empire.API.Data.Connection        (Connection)
import           Empire.API.Data.Connection        as Connection
import           Empire.API.Data.GraphLocation     (GraphLocation)
import           Empire.API.Data.Graph             (Graph)
import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.NodeMeta          (NodeMeta)
import           Empire.API.Data.PortRef           (InPortRef, OutPortRef, dstNodeId, srcNodeId)
import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.AddSubgraph      as AddSubgraph
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.Disconnect       as Disconnect
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


type Handler = ByteString -> UndoPure ()

handlersMap :: Map String (Handler)
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

data ResponseErrorException = ResponseErrorException deriving (Show)
instance Exception ResponseErrorException

makeHandler :: forall req inv res z. (Topic.MessageTopic (Response req inv res), Binary (Response req inv res),
            Topic.MessageTopic (Request (UndoResponseRequest (Response req inv res))), Binary (UndoResponseRequest (Response req inv res)),
            Topic.MessageTopic (Request (RedoResponseRequest (Response req inv res))), Binary (RedoResponseRequest (Response req inv res)))
            => (Response req inv res -> Maybe (UndoRequests (Response req inv res))) -> (String, Handler)
makeHandler h =
    let process content = let response   = decode . fromStrict $ content
                              maybeGuiID = response ^. Response.guiID
                              reqUUID    = response ^. Response.requestId
                          in forM_ maybeGuiID $ \guiId -> do
                              case h response of
                                      Nothing     -> throwM ResponseErrorException
                                      Just (r, q) -> do
                                          let message = UndoMessage guiId reqUUID (Topic.topic (Request.Request UUID.nil Nothing r)) r (Topic.topic (Request.Request UUID.nil Nothing q)) q
                                          handle message
    in (Topic.topic (undefined :: Response.Response req inv res), process)
    -- FIXME[WD]: nie uzywamy undefined, nigdy

compareMsgByUserId :: UndoMessage -> UndoMessage -> Bool
compareMsgByUserId msg1 msg2 = case msg1 of UndoMessage user1 _ _ _ _ _ -> case msg2 of UndoMessage user2 _ _ _ _ _ -> user1 == user2

handle :: UndoMessage -> UndoPure ()
handle message = do
    undo    %= (message :)
    redo    %= List.deleteBy compareMsgByUserId message
    history %= (message :)

withOk :: Response.Status a -> (a -> Maybe b) -> Maybe b
withOk (Response.Error _) _ = Nothing
withOk (Response.Ok a)    f = f a

handleAddNodeUndo :: AddNode.Response -> Maybe (RemoveNodes.Request, AddNode.Request)
handleAddNodeUndo (Response.Response _ _ (AddNode.Request location nodeType nodeMeta connectTo _) _ res) =
    withOk res $ \node ->
        let nodeId  = node ^. Node.nodeId
            undoMsg = RemoveNodes.Request location [nodeId]
            redoMsg = AddNode.Request location nodeType nodeMeta connectTo $ Just nodeId
        in Just (undoMsg, redoMsg)

handleAddSubgraphUndo :: AddSubgraph.Response -> Maybe (RemoveNodes.Request, AddSubgraph.Request)
handleAddSubgraphUndo (Response.Response _ _ (AddSubgraph.Request location nodes connections saveNodeIds) _ res ) =
    withOk res $ \idMapping ->
        case idMapping of
            Nothing     -> let ids     = map (^. Node.nodeId) nodes
                               undoMsg = RemoveNodes.Request location ids
                               redoMsg = AddSubgraph.Request location nodes connections True
                           in Just (undoMsg, redoMsg)
            Just idsMap -> let nodes'       = flip map nodes $ Node.nodeId %~ (idsMap Map.!)
                               connections' = map ((Connection.src . PortRef.srcNodeId %~ (idsMap Map.!))
                                                 . (Connection.dst . PortRef.dstNodeId %~ (idsMap Map.!))
                                                  ) connections
                               ids'    = map (^. Node.nodeId) nodes'
                               undoMsg = RemoveNodes.Request location ids'
                               redoMsg = AddSubgraph.Request location nodes' connections' True
                           in Just (undoMsg, redoMsg)

connect :: (OutPortRef, InPortRef) -> Connection
connect = uncurry Connection

filterNodes :: [NodeId] -> [Node] -> [Node]
filterNodes nodesIds nodes = catMaybes getNodes
    where getNode nId = List.find (\node -> node ^. Node.nodeId == nId) nodes
          getNodes    = map getNode nodesIds

filterConnections :: [(OutPortRef, InPortRef)] -> [NodeId] -> [(OutPortRef, InPortRef)]
filterConnections connectionPorts nodesIds = concat nodesConnections
    where  nodeConnections nId = Prologue.filter (\(outPort, inPort) -> outPort ^. PortRef.srcNodeId == nId
                                                                     || inPort  ^. PortRef.dstNodeId == nId) connectionPorts
           nodesConnections    = map nodeConnections nodesIds

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
filterMeta allNodes updates = map tupleUpdate justNodes
    where  nodeIds = map fst updates
           findNode updatedId = List.find (\node -> node ^. Node.nodeId == updatedId) allNodes
           justNodes          = catMaybes $ map findNode nodeIds

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
        Nothing      -> let undoMsg = RenameNode.Request location nodeId "" -- FIXME [WD]: nie uzywamy literalow, nigdy - jak to node moze nie miec nazwy
                            redoMsg = RenameNode.Request location nodeId name
                        in (undoMsg, redoMsg)
        Just nameOld -> let undoMsg = RenameNode.Request location nodeId nameOld
                            redoMsg = RenameNode.Request location nodeId name
                        in (undoMsg, redoMsg)

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
