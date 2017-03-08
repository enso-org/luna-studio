{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Handlers where

import           UndoState

import           Control.Exception                     (Exception)
import           Control.Exception.Safe                (MonadThrow, throwM)
import           Data.Binary                           (Binary, decode)
import qualified Data.Binary                           as Bin
import           Data.ByteString                       (ByteString, empty)
import           Data.ByteString.Lazy                  (fromStrict, toStrict)
import qualified Data.List                             as List
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import           Data.Maybe
import qualified Data.Set                              as Set
import           Prologue                              hiding (throwM)

import           Data.UUID                             as UUID (nil)
import           Empire.API.Data.Connection            (Connection)
import           Empire.API.Data.Connection            as Connection
import           Empire.API.Data.Graph                 (Graph)
import           Empire.API.Data.GraphLocation         (GraphLocation)
import           Empire.API.Data.Node                  (Node, NodeId)
import qualified Empire.API.Data.Node                  as Node
import           Empire.API.Data.NodeMeta              (NodeMeta)
import qualified Empire.API.Data.Port                  as Port
import           Empire.API.Data.PortRef               (AnyPortRef (OutPortRef'), InPortRef, OutPortRef (..), dstNodeId, srcNodeId)
import qualified Empire.API.Data.PortRef               as PortRef
import qualified Empire.API.Graph.AddNode              as AddNode
import qualified Empire.API.Graph.AddPort              as AddPort
import qualified Empire.API.Graph.AddSubgraph          as AddSubgraph
import qualified Empire.API.Graph.Connect              as Connect
import qualified Empire.API.Graph.Disconnect           as Disconnect
import qualified Empire.API.Graph.Redo                 as RedoRequest
import qualified Empire.API.Graph.RemoveNodes          as RemoveNodes
import qualified Empire.API.Graph.RemovePort           as RemovePort
import qualified Empire.API.Graph.RenameNode           as RenameNode
import qualified Empire.API.Graph.RenamePort           as RenamePort
import qualified Empire.API.Graph.SetCode              as SetCode
import qualified Empire.API.Graph.Undo                 as UndoRequest
import qualified Empire.API.Graph.UpdateNodeExpression as UpdateNodeExpression
import           Empire.API.Graph.UpdateNodeMeta       (SingleUpdate)
import qualified Empire.API.Graph.UpdateNodeMeta       as UpdateNodeMeta
import           Empire.API.Request                    (Request (..))
import qualified Empire.API.Request                    as Request
import           Empire.API.Response                   (Response (..))
import qualified Empire.API.Response                   as Response
import qualified Empire.API.Topic                      as Topic


type Handler = ByteString -> UndoPure ()

handlersMap :: Map String (Handler)
handlersMap = Map.fromList
    [ makeHandler handleAddNodeUndo
    -- , makeHandler handleAddPortUndo
    , makeHandler handleAddSubgraphUndo
    , makeHandler handleConnectUndo
    , makeHandler handleDisconnectUndo
    , makeHandler handleRemoveNodesUndo
    , makeHandler handleRenameNodeUndo
    , makeHandler handleRenamePortUndo
    , makeHandler handleSetCodeUndo
    , makeHandler handleUpdateNodeExpressionUndo
    , makeHandler handleUpdateNodeMetaUndo
    ]

type UndoRequests a = (UndoResponseRequest a, RedoResponseRequest a)

type family UndoResponseRequest t where
    UndoResponseRequest AddNode.Response              = RemoveNodes.Request
    UndoResponseRequest AddPort.Response              = RemovePort.Request
    UndoResponseRequest AddSubgraph.Response          = RemoveNodes.Request
    UndoResponseRequest Connect.Response              = Disconnect.Request
    UndoResponseRequest Disconnect.Response           = Connect.Request
    UndoResponseRequest RemoveNodes.Response          = AddSubgraph.Request
    UndoResponseRequest RenameNode.Response           = RenameNode.Request
    UndoResponseRequest RenamePort.Response           = RenamePort.Request
    UndoResponseRequest SetCode.Response              = SetCode.Request
    UndoResponseRequest UpdateNodeExpression.Response = UpdateNodeExpression.Request
    UndoResponseRequest UpdateNodeMeta.Response       = UpdateNodeMeta.Request

type family RedoResponseRequest t where
    RedoResponseRequest AddNode.Response              = AddNode.Request
    RedoResponseRequest AddPort.Response              = AddPort.Request
    RedoResponseRequest AddSubgraph.Response          = AddSubgraph.Request
    RedoResponseRequest Connect.Response              = Connect.Request
    RedoResponseRequest Disconnect.Response           = Disconnect.Request
    RedoResponseRequest RemoveNodes.Response          = RemoveNodes.Request
    RedoResponseRequest RenameNode.Response           = RenameNode.Request
    RedoResponseRequest RenamePort.Response           = RenamePort.Request
    RedoResponseRequest SetCode.Response              = SetCode.Request
    RedoResponseRequest UpdateNodeExpression.Response = UpdateNodeExpression.Request
    RedoResponseRequest UpdateNodeMeta.Response       = UpdateNodeMeta.Request

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


getUndoAddNode :: AddNode.Request -> RemoveNodes.Request
getUndoAddNode (AddNode.Request location nodeId _ _ _) =
    RemoveNodes.Request location [nodeId]

guiRevertAddNode :: AddNode.Request -> RemoveNodes.Request
guiRevertAddNode = getUndoAddNode

handleAddNodeUndo :: AddNode.Response -> Maybe (RemoveNodes.Request, AddNode.Request)
handleAddNodeUndo (Response.Response _ _ req@(AddNode.Request _ _ _ _ _) _ (Response.Ok _)) =
    Just (getUndoAddNode req, req)


getUndoAddSubgraph :: AddSubgraph.Request -> RemoveNodes.Request
getUndoAddSubgraph (AddSubgraph.Request location nodes conns) =
    RemoveNodes.Request location $ map (view Node.nodeId) nodes

guiRevertAddSubgraph :: AddSubgraph.Request -> RemoveNodes.Request
guiRevertAddSubgraph = getUndoAddSubgraph

handleAddSubgraphUndo :: AddSubgraph.Response -> Maybe (RemoveNodes.Request, AddSubgraph.Request)
handleAddSubgraphUndo (Response.Response _ _ req@(AddSubgraph.Request _ _ _) _ (Response.Ok _)) =
    Just (getUndoAddSubgraph req, req)


getUndoRemoveNodes :: RemoveNodes.Request -> RemoveNodes.Inverse -> AddSubgraph.Request
getUndoRemoveNodes (RemoveNodes.Request location _) (RemoveNodes.Inverse nodes conns) =
    AddSubgraph.Request location nodes $ map (uncurry Connection) conns

guiRevertRemoveNodes :: RemoveNodes.Request -> RemoveNodes.Inverse -> AddSubgraph.Request
guiRevertRemoveNodes = getUndoRemoveNodes

handleRemoveNodesUndo :: RemoveNodes.Response -> Maybe (AddSubgraph.Request, RemoveNodes.Request)
handleRemoveNodesUndo (Response.Response _ _ req@(RemoveNodes.Request _ _) (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoRemoveNodes req inv, req)


getUndoUpdateNodeExpression :: UpdateNodeExpression.Request -> UpdateNodeExpression.Inverse -> UpdateNodeExpression.Request
getUndoUpdateNodeExpression (UpdateNodeExpression.Request location nodeId _) (UpdateNodeExpression.Inverse prevExpr) =
    UpdateNodeExpression.Request location nodeId prevExpr

guiRevertUpdateNodeExpression :: UpdateNodeExpression.Request -> UpdateNodeExpression.Inverse -> UpdateNodeExpression.Request
guiRevertUpdateNodeExpression = getUndoUpdateNodeExpression

handleUpdateNodeExpressionUndo :: UpdateNodeExpression.Response -> Maybe ( UpdateNodeExpression.Request,  UpdateNodeExpression.Request)
handleUpdateNodeExpressionUndo (Response.Response _ _ req@(UpdateNodeExpression.Request _ _ _) (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoUpdateNodeExpression req inv, req)


getUndoUpdateNodeMeta :: UpdateNodeMeta.Request -> UpdateNodeMeta.Inverse -> UpdateNodeMeta.Request
getUndoUpdateNodeMeta (UpdateNodeMeta.Request location _) (UpdateNodeMeta.Inverse prevMeta) =
    UpdateNodeMeta.Request location prevMeta

guiRevertUpdateNodeMeta :: UpdateNodeMeta.Request -> UpdateNodeMeta.Inverse -> UpdateNodeMeta.Request
guiRevertUpdateNodeMeta = getUndoUpdateNodeMeta

handleUpdateNodeMetaUndo :: UpdateNodeMeta.Response -> Maybe (UpdateNodeMeta.Request, UpdateNodeMeta.Request)
handleUpdateNodeMetaUndo (Response.Response _ _ req@(UpdateNodeMeta.Request _ _) (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoUpdateNodeMeta req inv, req)


getUndoRenameNode :: RenameNode.Request -> RenameNode.Inverse -> RenameNode.Request
getUndoRenameNode (RenameNode.Request location nodeId _) (RenameNode.Inverse prevName) =
    RenameNode.Request location nodeId prevName

guiRevertRenameNode :: RenameNode.Request -> RenameNode.Inverse -> RenameNode.Request
guiRevertRenameNode = getUndoRenameNode

handleRenameNodeUndo :: RenameNode.Response -> Maybe (RenameNode.Request, RenameNode.Request)
handleRenameNodeUndo (Response.Response _ _ req@(RenameNode.Request _ _ _) (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoRenameNode req inv, req)


getUndoRenamePort :: RenamePort.Request -> RenamePort.Inverse -> RenamePort.Request
getUndoRenamePort (RenamePort.Request location portRef _) (RenamePort.Inverse prevName) =
    RenamePort.Request location portRef prevName

guiRevertRenamePort :: RenamePort.Request -> RenamePort.Inverse -> RenamePort.Request
guiRevertRenamePort = getUndoRenamePort

handleRenamePortUndo :: RenamePort.Response -> Maybe (RenamePort.Request, RenamePort.Request)
handleRenamePortUndo (Response.Response _ _ req@(RenamePort.Request _ _ _) (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoRenamePort req inv, req)


getUndoSetCode :: SetCode.Request -> SetCode.Inverse -> SetCode.Request
getUndoSetCode (SetCode.Request location nodeId _) (SetCode.Inverse prevCode) =
    SetCode.Request location nodeId prevCode

guiRevertSetCode :: SetCode.Request -> SetCode.Inverse -> SetCode.Request
guiRevertSetCode = getUndoSetCode

handleSetCodeUndo :: SetCode.Response -> Maybe (SetCode.Request, SetCode.Request)
handleSetCodeUndo (Response.Response _ _ req@(SetCode.Request _ _ _) (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoSetCode req inv, req)


getUndoConnect :: Connect.Request -> Connect.Result -> Disconnect.Request
getUndoConnect (Connect.Request location _ _) conn =
    Disconnect.Request location $ conn ^. Connection.dst

guiRevertConnect :: Connect.Request -> Maybe Disconnect.Request
guiRevertConnect (Connect.Request location _ (Left dst)) = Just $ Disconnect.Request location dst
guiRevertConnect _ = Nothing

handleConnectUndo :: Connect.Response -> Maybe (Disconnect.Request, Connect.Request)
handleConnectUndo (Response.Response _ _ req@(Connect.Request _ _ _) _ (Response.Ok res)) =
    Just (getUndoConnect req res, req)


getUndoDisconnect :: Disconnect.Request -> Disconnect.Inverse -> Connect.Request
getUndoDisconnect (Disconnect.Request location dst) (Disconnect.Inverse src) =
    Connect.Request location (Left src) (Left dst)

guiRevertDisconnect :: Disconnect.Request -> Disconnect.Inverse -> Connect.Request
guiRevertDisconnect = getUndoDisconnect

handleDisconnectUndo :: Disconnect.Response -> Maybe (Connect.Request, Disconnect.Request)
handleDisconnectUndo (Response.Response _ _ req@(Disconnect.Request _ _) (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoDisconnect req inv, req)
