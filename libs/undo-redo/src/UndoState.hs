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

module UndoState where


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


newtype Undo' b a = Undo {runUndo :: StateT UndoState b a}
    deriving (Applicative, Functor, Monad, MonadState UndoState, MonadIO, MonadThrow)

type Undo = Undo' Bus.BusT
type UndoPure = Undo' IO

data Action where
    Action :: (Binary req) => Topic.Topic -> req -> Action

data Act = ActUndo | ActRedo
