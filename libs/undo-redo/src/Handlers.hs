{-# LANGUAGE AllowAmbiguousTypes       #-}
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
import           Control.Lens                            ((.=), (%=), (^..),
                                                            _Right, to)
import           Data.Binary                             (Binary, decode)
import           Data.ByteString.Lazy                    (ByteString, fromStrict)
import qualified Data.List                               as List
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.Maybe
import           Data.UUID                               as UUID (nil)
import qualified LunaStudio.API.Atom.SetProject          as SetProject
import qualified LunaStudio.API.Atom.Substitute          as Substitute
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
import qualified LunaStudio.API.Graph.Transaction        as Transaction
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
    , makeHandler $ autoHandle @AddPort.Request
    , makeHandler $ autoHandle @AddSubgraph.Request
    , makeHandler $ autoHandle @AutolayoutNodes.Request
    , makeHandler $ autoHandle @CollapseToFunction.Request
    , makeHandler $ autoHandle @MovePort.Request
    , makeHandler handlePasteUndo
    , makeHandler $ autoHandle @RemoveConnection.Request
    , makeHandler $ autoHandle @RemoveNodes.Request
    , makeHandler $ autoHandle @RemovePort.Request
    , makeHandler $ autoHandle @RenameNode.Request
    , makeHandler $ autoHandle @RenamePort.Request
    , makeHandler $ autoHandle @SetCode.Request
    , makeHandler $ autoHandle @SetNodeExpression.Request
    , makeHandler $ autoHandle @SetNodesMeta.Request
    , makeHandler $ autoHandle @SetPortDefault.Request
    , makeHandler $ autoHandle @Substitute.Request
    , makeHandler $ autoHandle @Transaction.Request
    , makeResetHandler @SetProject.Request
    ]

type UndoRequests a = (UndoResponseRequest a, RedoResponseRequest a)

type family UndoReqRequest t where
    UndoReqRequest Paste.Request = RemoveNodes.Request
    UndoReqRequest a             = InverseOf a

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

makeResetHandler :: forall a. Topic.MessageTopic a => (String, Handler)
makeResetHandler = (Topic.topic @(Request a), clearState) where
    clearState _ = do
        undo    .= []
        redo    .= []
        history .= []

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

getUndoPaste :: Paste.Request -> Diff -> RemoveNodes.Request
getUndoPaste request (Diff mods)
    = RemoveNodes.Request (request ^. Paste.location) addedNodesLocs where
        toMaybeNodeLoc (Diff.AddNode m) = Just . convert
            $ m ^. Diff.newNode . Node.nodeId
        toMaybeNodeLoc _                = Nothing
        addedNodesLocs                  = catMaybes $ toMaybeNodeLoc <$> mods

handlePasteUndo :: ResponseOf Paste.Request -> Maybe (RemoveNodes.Request, Paste.Request)
handlePasteUndo (Response.Response _ _ req _ status) = case status of
    Response.Ok rsp -> Just (getUndoPaste req rsp, req)
    _               -> Nothing
