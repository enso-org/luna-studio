{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Handlers where

import           UndoState

import           Control.Exception                  (Exception)
import           Control.Exception.Safe             (MonadThrow, throwM)
import           Data.Binary                        (Binary, decode)
import qualified Data.Binary                        as Bin
import           Data.ByteString                    (ByteString, empty)
import           Data.ByteString.Lazy               (fromStrict, toStrict)
import qualified Data.List                          as List
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Maybe
import qualified Data.Set                           as Set
import           Prologue                           hiding (throwM)

import           Data.UUID                          as UUID (nil)
import           Empire.API.Data.Connection         (Connection)
import           Empire.API.Data.Connection         as Connection
import           Empire.API.Data.Graph              (Graph)
import           Empire.API.Data.GraphLocation      (GraphLocation)
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as Node
import           Empire.API.Data.NodeMeta           (NodeMeta)
import           Empire.API.Data.Port               (OutPortIndex (Projection))
import qualified Empire.API.Data.Port               as Port
import           Empire.API.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef (..), dstNodeId, srcNodeId,
                                                     toAnyPortRef)
import qualified Empire.API.Data.PortRef            as PortRef
import qualified Empire.API.Graph.AddConnection     as AddConnection
import qualified Empire.API.Graph.AddNode           as AddNode
import qualified Empire.API.Graph.AddPort           as AddPort
import qualified Empire.API.Graph.AddSubgraph       as AddSubgraph
import qualified Empire.API.Graph.MovePort          as MovePort
import qualified Empire.API.Graph.Redo              as RedoRequest
import qualified Empire.API.Graph.RemoveConnection  as RemoveConnection
import qualified Empire.API.Graph.RemoveNodes       as RemoveNodes
import qualified Empire.API.Graph.RemovePort        as RemovePort
import qualified Empire.API.Graph.RenameNode        as RenameNode
import qualified Empire.API.Graph.RenamePort        as RenamePort
import qualified Empire.API.Graph.SetNodeCode       as SetNodeCode
import qualified Empire.API.Graph.SetNodeExpression as SetNodeExpression
import           Empire.API.Graph.SetNodesMeta      (SingleUpdate)
import qualified Empire.API.Graph.SetNodesMeta      as SetNodesMeta
import qualified Empire.API.Graph.SetPortDefault    as SetPortDefault
import qualified Empire.API.Graph.Undo              as UndoRequest
import           Empire.API.Request                 (Request (..))
import qualified Empire.API.Request                 as Request
import           Empire.API.Response                (Response (..))
import qualified Empire.API.Response                as Response
import qualified Empire.API.Topic                   as Topic


type Handler = ByteString -> UndoPure ()

handlersMap :: Map String (Handler)
handlersMap = Map.fromList
    [ makeHandler handleAddConnectionUndo
    , makeHandler handleAddNodeUndo
    , makeHandler handleAddPortUndo
    , makeHandler handleAddSubgraphUndo
    , makeHandler handleMovePortUndo
    , makeHandler handleRemoveConnectionUndo
    , makeHandler handleRemoveNodesUndo
    , makeHandler handleRemovePortUndo
    , makeHandler handleRenameNodeUndo
    , makeHandler handleRenamePortUndo
    , makeHandler handleSetNodeCodeUndo
    , makeHandler handleSetNodeExpressionUndo
    , makeHandler handleSetNodesMetaUndo
    , makeHandler handleSetPortDefaultUndo
    ]

type UndoRequests a = (UndoResponseRequest a, RedoResponseRequest a)

type family UndoResponseRequest t where
    UndoResponseRequest AddConnection.Response        = RemoveConnection.Request
    UndoResponseRequest AddNode.Response              = RemoveNodes.Request
    UndoResponseRequest AddPort.Response              = RemovePort.Request
    UndoResponseRequest AddSubgraph.Response          = RemoveNodes.Request
    UndoResponseRequest MovePort.Response             = MovePort.Request
    UndoResponseRequest RemoveConnection.Response     = AddConnection.Request
    UndoResponseRequest RemoveNodes.Response          = AddSubgraph.Request
    UndoResponseRequest RemovePort.Response           = AddPort.Request
    UndoResponseRequest RenameNode.Response           = RenameNode.Request
    UndoResponseRequest RenamePort.Response           = RenamePort.Request
    UndoResponseRequest SetNodeCode.Response          = SetNodeCode.Request
    UndoResponseRequest SetNodeExpression.Response    = SetNodeExpression.Request
    UndoResponseRequest SetNodesMeta.Response         = SetNodesMeta.Request
    UndoResponseRequest SetPortDefault.Response       = SetPortDefault.Request

type family RedoResponseRequest t where
    RedoResponseRequest AddConnection.Response        = AddConnection.Request
    RedoResponseRequest AddNode.Response              = AddNode.Request
    RedoResponseRequest AddPort.Response              = AddPort.Request
    RedoResponseRequest AddSubgraph.Response          = AddSubgraph.Request
    RedoResponseRequest MovePort.Response             = MovePort.Request
    RedoResponseRequest RemoveConnection.Response     = RemoveConnection.Request
    RedoResponseRequest RemoveNodes.Response          = RemoveNodes.Request
    RedoResponseRequest RemovePort.Response           = RemovePort.Request
    RedoResponseRequest RenameNode.Response           = RenameNode.Request
    RedoResponseRequest RenamePort.Response           = RenamePort.Request
    RedoResponseRequest SetNodeCode.Response          = SetNodeCode.Request
    RedoResponseRequest SetNodeExpression.Response    = SetNodeExpression.Request
    RedoResponseRequest SetNodesMeta.Response         = SetNodesMeta.Request
    RedoResponseRequest SetPortDefault.Response       = SetPortDefault.Request

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
getUndoAddNode (AddNode.Request location nodeLoc _ _ _) =
    RemoveNodes.Request location [nodeLoc]

handleAddNodeUndo :: AddNode.Response -> Maybe (RemoveNodes.Request, AddNode.Request)
handleAddNodeUndo (Response.Response _ _ req _ (Response.Ok _)) =
    Just (getUndoAddNode req, req)


getUndoAddPort :: AddPort.Request -> RemovePort.Request
getUndoAddPort (AddPort.Request location portRef connections) =
    RemovePort.Request location portRef

handleAddPortUndo :: AddPort.Response -> Maybe (RemovePort.Request, AddPort.Request)
handleAddPortUndo (Response.Response _ _ req _ (Response.Ok _)) = Just (getUndoAddPort req, req)


getUndoAddSubgraph :: AddSubgraph.Request -> RemoveNodes.Request
getUndoAddSubgraph (AddSubgraph.Request location nodes conns) =
    RemoveNodes.Request location $ map (convert . view Node.nodeId) nodes

handleAddSubgraphUndo :: AddSubgraph.Response -> Maybe (RemoveNodes.Request, AddSubgraph.Request)
handleAddSubgraphUndo (Response.Response _ _ req _ (Response.Ok _)) =
    Just (getUndoAddSubgraph req, req)


getUndoAddConnection :: AddConnection.Request -> AddConnection.Result -> RemoveConnection.Request
getUndoAddConnection (AddConnection.Request location _ _) conn =
    RemoveConnection.Request location $ conn ^. Connection.dst

handleAddConnectionUndo :: AddConnection.Response -> Maybe (RemoveConnection.Request, AddConnection.Request)
handleAddConnectionUndo (Response.Response _ _ req _ (Response.Ok res)) =
    Just (getUndoAddConnection req res, req)


getUndoMovePort :: MovePort.Request -> MovePort.Request
getUndoMovePort (MovePort.Request location oldPortRef newPos) = case oldPortRef of
    OutPortRef nid (Projection i : rest) ->
        MovePort.Request location (OutPortRef nid $ Projection newPos : rest) i

handleMovePortUndo :: MovePort.Response -> Maybe (MovePort.Request, MovePort.Request)
handleMovePortUndo (Response.Response _ _ req _ (Response.Ok _)) =
    Just (getUndoMovePort req, req)


getUndoRemoveConnection :: RemoveConnection.Request -> RemoveConnection.Inverse -> AddConnection.Request
getUndoRemoveConnection (RemoveConnection.Request location dst) (RemoveConnection.Inverse src) =
    AddConnection.Request location (Left src) (Left $ InPortRef' dst)

handleRemoveConnectionUndo :: RemoveConnection.Response -> Maybe (AddConnection.Request, RemoveConnection.Request)
handleRemoveConnectionUndo (Response.Response _ _ req (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoRemoveConnection req inv, req)


getUndoRemoveNodes :: RemoveNodes.Request -> RemoveNodes.Inverse -> AddSubgraph.Request
getUndoRemoveNodes (RemoveNodes.Request location _) (RemoveNodes.Inverse nodes conns) =
    AddSubgraph.Request location nodes conns

handleRemoveNodesUndo :: RemoveNodes.Response -> Maybe (AddSubgraph.Request, RemoveNodes.Request)
handleRemoveNodesUndo (Response.Response _ _ req (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoRemoveNodes req inv, req)


-- TODO[LJK/SB]: Preserve connections
getUndoRemovePort :: RemovePort.Request -> RemovePort.Inverse -> AddPort.Request
getUndoRemovePort (RemovePort.Request location portRef) (RemovePort.Inverse conns) =
    AddPort.Request location portRef $ Just conns

handleRemovePortUndo :: RemovePort.Response -> Maybe (AddPort.Request, RemovePort.Request)
handleRemovePortUndo (Response.Response _ _ req (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoRemovePort req inv, req)


getUndoRenameNode :: RenameNode.Request -> RenameNode.Inverse -> RenameNode.Request
getUndoRenameNode (RenameNode.Request location nodeId _) (RenameNode.Inverse prevName) =
    RenameNode.Request location nodeId prevName

handleRenameNodeUndo :: RenameNode.Response -> Maybe (RenameNode.Request, RenameNode.Request)
handleRenameNodeUndo (Response.Response _ _ req (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoRenameNode req inv, req)


getUndoRenamePort :: RenamePort.Request -> RenamePort.Inverse -> RenamePort.Request
getUndoRenamePort (RenamePort.Request location portRef _) (RenamePort.Inverse prevName) =
    RenamePort.Request location portRef prevName

handleRenamePortUndo :: RenamePort.Response -> Maybe (RenamePort.Request, RenamePort.Request)
handleRenamePortUndo (Response.Response _ _ req (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoRenamePort req inv, req)


getUndoSetNodeCode :: SetNodeCode.Request -> SetNodeCode.Inverse -> SetNodeCode.Request
getUndoSetNodeCode (SetNodeCode.Request location nodeId _) (SetNodeCode.Inverse prevCode) =
    SetNodeCode.Request location nodeId prevCode

handleSetNodeCodeUndo :: SetNodeCode.Response -> Maybe (SetNodeCode.Request, SetNodeCode.Request)
handleSetNodeCodeUndo (Response.Response _ _ req (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoSetNodeCode req inv, req)


getUndoSetNodeExpression :: SetNodeExpression.Request -> SetNodeExpression.Inverse -> SetNodeExpression.Request
getUndoSetNodeExpression (SetNodeExpression.Request location nodeId _) (SetNodeExpression.Inverse prevExpr) =
    SetNodeExpression.Request location nodeId prevExpr

handleSetNodeExpressionUndo :: SetNodeExpression.Response -> Maybe ( SetNodeExpression.Request,  SetNodeExpression.Request)
handleSetNodeExpressionUndo (Response.Response _ _ req (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoSetNodeExpression req inv, req)


getUndoSetNodesMeta :: SetNodesMeta.Request -> SetNodesMeta.Inverse -> SetNodesMeta.Request
getUndoSetNodesMeta (SetNodesMeta.Request location _) (SetNodesMeta.Inverse prevMeta) =
    SetNodesMeta.Request location prevMeta

handleSetNodesMetaUndo :: SetNodesMeta.Response -> Maybe (SetNodesMeta.Request, SetNodesMeta.Request)
handleSetNodesMetaUndo (Response.Response _ _ req (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoSetNodesMeta req inv, req)


getUndoSetPortDefault :: SetPortDefault.Request -> SetPortDefault.Inverse -> SetPortDefault.Request
getUndoSetPortDefault (SetPortDefault.Request location portRef _) (SetPortDefault.Inverse prevPortDefault) =
    SetPortDefault.Request location portRef prevPortDefault

handleSetPortDefaultUndo :: SetPortDefault.Response -> Maybe (SetPortDefault.Request, SetPortDefault.Request)
handleSetPortDefaultUndo (Response.Response _ _ req (Response.Ok inv) (Response.Ok _)) =
    Just (getUndoSetPortDefault req inv, req)
