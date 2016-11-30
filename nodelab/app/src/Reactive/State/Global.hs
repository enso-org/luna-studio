{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reactive.State.Global where

import           Data.Word                      (Word8)
import           Utils.PreludePlus
import           Utils.Vector

import           Data.Aeson                     (ToJSON, toJSON)
import           Data.DateTime                  (DateTime)
import           Data.Set                       (Set)
import           Data.UUID.Types                (UUID)
import           Reactive.Commands.Command      (Command)
import           System.Random                  (StdGen)
import qualified System.Random                  as Random

import           Batch.Workspace
import           Empire.API.Data.Connection     (ConnectionId)
import           Empire.API.Data.Node           (NodeId)
import qualified Empire.API.Graph.Collaboration as Collaboration
import qualified Event.Event                    as Event
import           React.Store                    (Ref)
import qualified React.Store                    as Store
import           React.Store.App                (App)
import qualified React.Store.App                as App
import           React.Store.CodeEditor         (CodeEditor)
import           React.Store.Connection         (Connection)
import           React.Store.Node               (Node)
import           React.Store.NodeEditor         (NodeEditor)
import qualified React.Store.NodeEditor         as NodeEditor
import qualified Reactive.State.Camera          as Camera
import qualified Reactive.State.Collaboration   as Collaboration
import qualified Reactive.State.Connect         as Connect
import qualified Reactive.State.ConnectionPen   as ConnectionPen
import qualified Reactive.State.Drag            as Drag
import qualified Reactive.State.Graph           as Graph
import qualified Reactive.State.MultiSelection  as MultiSelection
import qualified Reactive.State.UIElements      as UIElements
import qualified Reactive.State.UIRegistry      as UIRegistry



foreign import javascript safe "{}" defJsState :: Event.JSState

data State = State { _mousePos           :: Vector2 Int
                   , _graph              :: Graph.State
                   , _camera             :: Camera.State
                   , _multiSelection     :: MultiSelection.State
                   , _drag               :: Drag.State
                   , _connect            :: Connect.State
                   , _uiRegistry         :: UIRegistry.State
                   , _connectionPen      :: ConnectionPen.State
                   , _workspace          :: Workspace
                   , _uiElements         :: UIElements.State
                   , _lastEvent          :: Maybe Event.Event
                   , _eventNum           :: Int
                   , _jsState            :: Event.JSState
                   , _collaboration      :: Collaboration.State
                   , _pendingRequests    :: Set UUID
                   , _lastEventTimestamp :: DateTime
                   , _clientId           :: Collaboration.ClientId
                   , _random             :: StdGen
                   , _tutorial           :: Maybe Int
                   , _app                :: Ref App
                   } deriving (Generic)

instance ToJSON State
instance ToJSON StdGen where
    toJSON _ = toJSON "(random-generator)"
instance ToJSON (Ref App) where
    toJSON _ = toJSON "((Ref App))"


makeLenses ''State

inApp :: (Ref App -> Command State r) -> Command State r
inApp action = action =<< use app

inNodeEditor :: (Ref NodeEditor -> Command State r) -> Command State r
inNodeEditor action = inApp $ (action . view App.nodeEditor) <=< Store.get

inCodeEditor :: (Ref CodeEditor -> Command State r) -> Command State r
inCodeEditor action = inApp $ (action . view App.codeEditor) <=< Store.get

inNode :: NodeId -> (Maybe (Ref Node) -> Command State r) -> Command State r
inNode nodeId action = inNodeEditor $ (action . view (NodeEditor.nodes . at nodeId)) <=< Store.get

getNode :: NodeId -> Command State (Maybe (Ref Node))
getNode nodeId = inNode nodeId return

inConnection :: ConnectionId -> (Maybe (Ref Connection) -> Command State r) -> Command State r
inConnection connectionId action =
    inNodeEditor $ (action . view (NodeEditor.connections . at connectionId)) <=< Store.get

getConnection :: ConnectionId -> Command State (Maybe (Ref Connection))
getConnection connectionId = inConnection connectionId return

initialState :: DateTime -> Collaboration.ClientId -> StdGen -> Maybe Int -> (Ref App) -> State
initialState = State (Vector2 200 200) def def def def def def def def def def def defJsState def def

inRegistry :: Command UIRegistry.State a -> Command State a
inRegistry = zoom uiRegistry

inRegistry_ :: Command UIRegistry.State a -> Command State ()
inRegistry_ = void . zoom uiRegistry

nextRandom :: Command State Word8
nextRandom = do
    (val, rnd) <- uses random Random.random
    random .= rnd
    return val
