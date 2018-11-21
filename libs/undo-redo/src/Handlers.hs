{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}

module Handlers where

import           UndoState

import           Control.Exception                       (Exception)
import           Control.Exception.Safe                  (throwM)
import           Control.Lens                            ((%=), (^..), _Right, to)
import           Data.Binary                             (Binary, decode)
import           Data.ByteString.Lazy                    (ByteString, fromStrict)
import qualified Data.List                               as List
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.Maybe
import           Data.UUID                               as UUID (nil)
import qualified LunaStudio.API.Graph.AddConnection      as AddConnection
import qualified LunaStudio.API.Graph.AddNode            as AddNode
import qualified LunaStudio.API.Graph.AddPort            as AddPort
import qualified LunaStudio.API.Graph.AddSubgraph        as AddSubgraph
import qualified LunaStudio.API.Graph.AutolayoutNodes    as AutolayoutNodes
import qualified LunaStudio.API.Graph.CollapseToFunction as CollapseToFunction
import qualified LunaStudio.API.Graph.MovePort           as MovePort
import qualified LunaStudio.API.Graph.Paste              as Paste
import qualified LunaStudio.API.Graph.RemoveConnection   as RemoveConnection
import qualified LunaStudio.API.Graph.RemoveNodes        as RemoveNodes
import qualified LunaStudio.API.Graph.RemovePort         as RemovePort
import qualified LunaStudio.API.Graph.RenameNode         as RenameNode
import qualified LunaStudio.API.Graph.RenamePort         as RenamePort
import qualified LunaStudio.API.Graph.SetCode            as SetCode
import qualified LunaStudio.API.Graph.SetNodeExpression  as SetNodeExpression
import qualified LunaStudio.API.Graph.SetNodesMeta       as SetNodesMeta
import qualified LunaStudio.API.Graph.SetPortDefault     as SetPortDefault
import           LunaStudio.API.Request                  (Request (..))
import qualified LunaStudio.API.Request                  as Request
import           LunaStudio.API.Response                 (Response (..), ResponseOf, InverseOf)
import qualified LunaStudio.API.Response                 as Response
import qualified LunaStudio.API.Topic                    as Topic
import           LunaStudio.Data.Connection              as Connection
import           LunaStudio.Data.Diff                    (Diff (Diff))
import qualified LunaStudio.Data.Diff                    as Diff
import qualified LunaStudio.Data.Graph                   as Graph
import qualified LunaStudio.Data.Node                    as Node
import qualified LunaStudio.Data.NodeLoc                 as NodeLoc
import           LunaStudio.Data.Port                    (OutPortIndex (Projection))
import           LunaStudio.Data.PortRef                 (AnyPortRef (InPortRef'), OutPortRef (..))
import qualified LunaStudio.Data.PortRef                 as PortRef
import qualified System.Log.MLogger                      as Logger
import           Prologue                                hiding (throwM)


logger :: Logger.Logger
logger = Logger.getLogger $(Logger.moduleName)


type Handler = ByteString -> UndoPure ()

handlersMap :: Map String Handler
handlersMap = fromList
    [ makeHandler $ autoHandle @AddConnection.Request
    , makeHandler $ autoHandle @AddNode.Request
    , makeHandler handleAddPortUndo
    , makeHandler handleAddSubgraphUndo
    , makeHandler handleAutolayoutNodes
    , makeHandler handleCollapseToFunctionUndo
    , makeHandler handleMovePortUndo
    , makeHandler handlePasteUndo
    , makeHandler handleRemoveConnectionUndo
    , makeHandler handleRemoveNodesUndo
    , makeHandler handleRemovePortUndo
    , makeHandler handleRenameNodeUndo
    , makeHandler handleRenamePortUndo
    , makeHandler handleSetCodeUndo
    , makeHandler $ autoHandle @SetNodeExpression.Request
    , makeHandler $ autoHandle @SetNodesMeta.Request
    , makeHandler handleSetPortDefaultUndo
    ]

type UndoRequests a = (UndoResponseRequest a, RedoResponseRequest a)

type family UndoReqRequest t where
    UndoReqRequest AddPort.Request              = RemovePort.Request
    UndoReqRequest AddSubgraph.Request          = RemoveNodes.Request
    UndoReqRequest AutolayoutNodes.Request      = SetNodesMeta.Request
    UndoReqRequest CollapseToFunction.Request   = SetCode.Request
    UndoReqRequest MovePort.Request             = MovePort.Request
    UndoReqRequest Paste.Request                = RemoveNodes.Request
    UndoReqRequest RemoveConnection.Request     = AddConnection.Request
    UndoReqRequest RemoveNodes.Request          = AddSubgraph.Request
    UndoReqRequest RemovePort.Request           = AddPort.Request
    UndoReqRequest RenameNode.Request           = RenameNode.Request
    UndoReqRequest RenamePort.Request           = RenamePort.Request
    UndoReqRequest SetCode.Request              = SetCode.Request
    UndoReqRequest SetPortDefault.Request       = SetPortDefault.Request
    UndoReqRequest a = InverseOf a

type family UndoResponseRequest t where
    UndoResponseRequest (Response req inv res) = UndoReqRequest req

type family RedoResponseRequest a where
    RedoResponseRequest (Response req inv res) = req

data ResponseErrorException = forall req inv res. (Show req, Show res, Show inv)
    => ResponseErrorException (Response req inv res)

deriving instance Show ResponseErrorException
instance Exception ResponseErrorException

makeHandler :: forall req inv res.
    ( Topic.MessageTopic (Response req inv res), Binary (Response req inv res)
    , Topic.MessageTopic (Request (UndoResponseRequest (Response req inv res)))
    , Binary (UndoResponseRequest (Response req inv res))
    , Topic.MessageTopic (Request (RedoResponseRequest (Response req inv res)))
    , Binary (RedoResponseRequest (Response req inv res))
    , Show req
    , Show inv
    , Show res
    ) => (Response req inv res -> Maybe (UndoRequests (Response req inv res)))
    -> (String, Handler)
makeHandler h = (Topic.topic @(Response.Response req inv res), process) where
    process content = do
        let response   = decode content
            maybeGuiID = response ^. Response.guiID
            reqUUID    = response ^. Response.requestId
        for_ maybeGuiID $ \guiId -> case h response of
            Nothing     -> throwM $ ResponseErrorException response
            Just (r, q) -> handle $ UndoMessage
                guiId
                reqUUID
                (Topic.topic' (Request.Request UUID.nil Nothing r))
                r
                (Topic.topic' (Request.Request UUID.nil Nothing q))
                q

compareMsgByUserId :: UndoMessage -> UndoMessage -> Bool
compareMsgByUserId msg1 msg2 = case msg1 of
    UndoMessage user1 _ _ _ _ _ -> case msg2 of
        UndoMessage user2 _ _ _ _ _ -> user1 == user2

handle :: UndoMessage -> UndoPure ()
handle message = do
    undo    %= (message :)
    redo    %= List.filter (not . compareMsgByUserId message)
    history %= (message :)

autoHandle :: forall req. ResponseOf req -> Maybe (InverseOf req, req)
autoHandle (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _) -> Just (inv, req)
        _ -> Nothing

getUndoAddPort :: AddPort.Request -> RemovePort.Request
getUndoAddPort (AddPort.Request location portRef connections _)
    = RemovePort.Request location portRef

handleAddPortUndo :: AddPort.Response
    -> Maybe (RemovePort.Request, AddPort.Request)
handleAddPortUndo (Response.Response _ _ req _ status) = case status of
    Response.Ok _ -> Just (getUndoAddPort req, req)
    _             -> Nothing


getUndoAddSubgraph :: AddSubgraph.Request -> RemoveNodes.Request
getUndoAddSubgraph (AddSubgraph.Request location nodes conns) =
    RemoveNodes.Request location $ map (convert . view Node.nodeId) nodes

handleAddSubgraphUndo :: AddSubgraph.Response
    -> Maybe (RemoveNodes.Request, AddSubgraph.Request)
handleAddSubgraphUndo (Response.Response _ _ req _ status) = case status of
    Response.Ok _ -> Just (getUndoAddSubgraph req, req)
    _             -> Nothing

getUndoAutolayout :: AutolayoutNodes.Request -> AutolayoutNodes.Inverse
    -> SetNodesMeta.Request
getUndoAutolayout
    (AutolayoutNodes.Request location _ _)
    (AutolayoutNodes.Inverse positions) = SetNodesMeta.Request
        location
        . fromList $ (& _1 %~ convert) <$> positions

handleAutolayoutNodes :: AutolayoutNodes.Response
    -> Maybe (SetNodesMeta.Request, AutolayoutNodes.Request)
handleAutolayoutNodes (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _)
            -> Just (getUndoAutolayout req inv, req)
        _   -> Nothing

handleCollapseToFunctionUndo :: CollapseToFunction.Response
    -> Maybe (SetCode.Request, CollapseToFunction.Request)
handleCollapseToFunctionUndo (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _) -> Just
            ( SetCode.Request
                (req ^. CollapseToFunction.location)
                (inv ^. CollapseToFunction.prevCode)
                (inv ^. CollapseToFunction.nodeCache)
            , req)
        _ -> Nothing


getUndoMovePort :: MovePort.Request -> MovePort.Request
getUndoMovePort (MovePort.Request location oldPortRef newPos) =
    case oldPortRef of
        OutPortRef nid (Projection i : rest) -> MovePort.Request
            location
            (OutPortRef nid $ Projection newPos : rest)
            i

handleMovePortUndo :: MovePort.Response -> Maybe (MovePort.Request, MovePort.Request)
handleMovePortUndo (Response.Response _ _ req _ status) = case status of
    Response.Ok _ -> Just (getUndoMovePort req, req)
    _             -> Nothing


getUndoPaste :: Paste.Request -> Diff -> RemoveNodes.Request
getUndoPaste request (Diff mods)
    = RemoveNodes.Request (request ^. Paste.location) addedNodesLocs where
        toMaybeNodeLoc (Diff.AddNode m) = Just . convert
            $ m ^. Diff.newNode . Node.nodeId
        toMaybeNodeLoc _                = Nothing
        addedNodesLocs                  = catMaybes $ toMaybeNodeLoc <$> mods

handlePasteUndo :: Paste.Response -> Maybe (RemoveNodes.Request, Paste.Request)
handlePasteUndo (Response.Response _ _ req _ status) = case status of
    Response.Ok rsp -> Just (getUndoPaste req rsp, req)
    _               -> Nothing


getUndoRemoveConnection :: RemoveConnection.Request -> RemoveConnection.Inverse
    -> AddConnection.Request
getUndoRemoveConnection
    (RemoveConnection.Request location dst)
    (RemoveConnection.Inverse src)
        = AddConnection.Request location (Left src) (Left $ InPortRef' dst)

handleRemoveConnectionUndo :: RemoveConnection.Response
    -> Maybe (AddConnection.Request, RemoveConnection.Request)
handleRemoveConnectionUndo (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _)
            -> Just (getUndoRemoveConnection req inv, req)
        _   -> Nothing


getUndoRemoveNodes :: RemoveNodes.Request -> RemoveNodes.Inverse
    -> AddSubgraph.Request
getUndoRemoveNodes
    (RemoveNodes.Request location _)
    (RemoveNodes.Inverse nodes conns) = AddSubgraph.Request location nodes conns

handleRemoveNodesUndo :: ResponseOf RemoveNodes.Request
    -> Maybe (AddSubgraph.Request, RemoveNodes.Request)
handleRemoveNodesUndo (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _)
            -> Just (getUndoRemoveNodes req inv, req)
        _   -> Nothing


-- TODO[LJK/SB]: Preserve connections
getUndoRemovePort :: RemovePort.Request -> RemovePort.Inverse -> AddPort.Request
getUndoRemovePort
    (RemovePort.Request location portRef)
    (RemovePort.Inverse oldName conns) = AddPort.Request
        location
        portRef
        (InPortRef' . view Connection.dst <$> conns)
        (Just oldName)

handleRemovePortUndo :: RemovePort.Response
    -> Maybe (AddPort.Request, RemovePort.Request)
handleRemovePortUndo (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _)
            -> Just (getUndoRemovePort req inv, req)
        _   -> Nothing


getUndoRenameNode :: RenameNode.Request -> RenameNode.Inverse
    -> RenameNode.Request
getUndoRenameNode
    (RenameNode.Request location nodeId _)
    (RenameNode.Inverse prevName) = RenameNode.Request location nodeId prevName

handleRenameNodeUndo :: RenameNode.Response
    -> Maybe (RenameNode.Request, RenameNode.Request)
handleRenameNodeUndo (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _)
            -> Just (getUndoRenameNode req inv, req)
        _   -> Nothing


getUndoRenamePort :: RenamePort.Request -> RenamePort.Inverse
    -> RenamePort.Request
getUndoRenamePort
    (RenamePort.Request location portRef _)
    (RenamePort.Inverse prevName) = RenamePort.Request location portRef prevName

handleRenamePortUndo :: RenamePort.Response
    -> Maybe (RenamePort.Request, RenamePort.Request)
handleRenamePortUndo (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _)
            -> Just (getUndoRenamePort req inv, req)
        _   -> Nothing

handleSetCodeUndo :: SetCode.Response
    -> Maybe (SetCode.Request,  SetCode.Request)
handleSetCodeUndo (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _) -> Just
            (SetCode.Request
                (req ^. SetCode.location)
                (inv ^. SetCode.prevCode)
                (inv ^. SetCode.prevCache)
            , req)
        _ -> Nothing

getUndoSetPortDefault :: SetPortDefault.Request -> SetPortDefault.Inverse
    -> SetPortDefault.Request
getUndoSetPortDefault
    (SetPortDefault.Request location portRef _)
    (SetPortDefault.Inverse prevPortDefault)
        = SetPortDefault.Request location portRef prevPortDefault

handleSetPortDefaultUndo :: SetPortDefault.Response
    -> Maybe (SetPortDefault.Request, SetPortDefault.Request)
handleSetPortDefaultUndo (Response.Response _ _ req invStatus status)
    = case (invStatus, status) of
        (Response.Ok inv, Response.Ok _)
            -> Just (getUndoSetPortDefault req inv, req)
        _   -> Nothing
