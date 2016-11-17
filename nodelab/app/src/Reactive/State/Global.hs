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
import qualified Empire.API.Graph.Collaboration as Collaboration
import qualified Event.Event                    as Event
import           React.Stores                   (Stores)
import qualified React.Stores                   as Stores
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
                   , _stores             :: Stores
                   } deriving (Generic)

instance ToJSON State
instance ToJSON StdGen where
    toJSON _ = toJSON "(random-generator)"
instance ToJSON Stores where
    toJSON _ = toJSON "(stores)"


makeLenses ''State

initialState :: DateTime -> Collaboration.ClientId -> StdGen -> Maybe Int -> Stores -> State
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
