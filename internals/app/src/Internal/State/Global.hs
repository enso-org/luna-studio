{-# OPTIONS_GHC -fno-warn-orphans #-}
module Internal.State.Global where

import           Data.Aeson                           (ToJSON, toJSON)
import           Data.DateTime                        (DateTime)
import           Data.Map                             (Map)
-- import           Data.ScreenPosition                  (ScreenPosition (ScreenPosition))
import           Data.Set                             (Set)
import           Data.UUID.Types                      (UUID)
-- import           Data.Vector                          (Vector2 (Vector2))
import           Data.Word                            (Word8)
import           Empire.API.Data.Node                 (NodeId)
import           Empire.API.Graph.CollaborationUpdate (ClientId)
-- import           JS.Scene                             (Scene)
import           Internal.Action.Command           (Command)
import           Internal.Batch.Workspace
import           Internal.Event.Event              (Event)
import           Internal.Prelude
-- import           Internal.React.Model.App          (App)
-- import           Internal.React.Store              (Ref)
-- import           Internal.State.Action             (ActionRep, Connect, SomeAction)
import           Internal.State.Collaboration      (Collaboration)
-- import           Internal.State.Graph              (Graph)
import           System.Random                        (StdGen)
import qualified System.Random                        as Random


-- TODO split to more states
-- TODO: Reconsider our design. @wdanilo says that we shouldn't use MonadState at all
data State = State { _workspace            :: Workspace
                   , _lastEvent            :: Maybe Event
                   , _eventNum             :: Int
                   , _collaboration        :: Collaboration
                   , _pendingRequests      :: Set UUID
                   , _lastEventTimestamp   :: DateTime
                   , _clientId             :: ClientId
                   , _random               :: StdGen
                   }

instance ToJSON StdGen where
    toJSON _ = toJSON "(random-generator)"
-- instance ToJSON (Ref App) where
--     toJSON _ = toJSON "(Ref App)"

makeLenses ''State

mkState :: DateTime -> ClientId -> StdGen -> State
mkState = State def def def def def

nextRandom :: Command State Word8
nextRandom = uses random Random.random >>= \(val, rnd) -> random .= rnd >> return val
