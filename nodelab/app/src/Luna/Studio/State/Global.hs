{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.State.Global where

import           Data.Aeson                           (ToJSON, toJSON)
import           Data.DateTime                        (DateTime)
import           Data.Map                             (Map)
import           Data.ScreenPosition                  (ScreenPosition (ScreenPosition))
import           Data.Set                             (Set)
import           Data.UUID.Types                      (UUID)
import           Data.Vector                          (Vector2 (Vector2))
import           Data.Word                            (Word8)
import           Empire.API.Data.Node                 (NodeId)
import           Empire.API.Graph.CollaborationUpdate (ClientId)
import           JS.Scene                             (Scene)
import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Batch.Workspace
import           Luna.Studio.Event.Event              (Event)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App          (App)
import           Luna.Studio.React.Store              (Ref)
import           Luna.Studio.State.Action             (ActionRep, Connect, SomeAction)
import           Luna.Studio.State.Collaboration      (Collaboration)
import           Luna.Studio.State.Graph              (Graph)
import           System.Random                        (StdGen)
import qualified System.Random                        as Random


-- TODO split to more states
-- TODO: Reconsider our design. @wdanilo says that we shouldn't use MonadState at all
data State = State { _mousePos             :: ScreenPosition
                   , _graph                :: Graph
                   , _renderNeeded         :: Bool --TODO refactor
                   , _currentActions       :: Map ActionRep (SomeAction (Command State))
                   -- TODO[LJK]: This is duplicate. Find way to remove it but make it possible to get Connect without importing its instance
                   , _currentConnectAction :: Maybe Connect
                   , _selectionHistory     :: [Set NodeId]
                   , _topZIndex            :: Int
                   , _scene                :: Maybe Scene
                   -- TODO[react]: wyjebawszy
                   , _workspace            :: Workspace
                   , _lastEvent            :: Maybe Event
                   , _eventNum             :: Int
                   , _collaboration        :: Collaboration
                   , _pendingRequests      :: Set UUID
                   , _lastEventTimestamp   :: DateTime
                   , _clientId             :: ClientId
                   , _random               :: StdGen
                   , _app                  :: Ref App
                   }

instance ToJSON StdGen where
    toJSON _ = toJSON "(random-generator)"
instance ToJSON (Ref App) where
    toJSON _ = toJSON "(Ref App)"

makeLenses ''State

mkState :: DateTime -> ClientId -> StdGen -> Ref App -> State
mkState = State (ScreenPosition (Vector2 200 200)) def False def def def def def def def def def def

nextRandom :: Command State Word8
nextRandom = uses random Random.random >>= \(val, rnd) -> random .= rnd >> return val
