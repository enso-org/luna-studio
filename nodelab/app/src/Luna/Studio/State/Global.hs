{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.State.Global where

import           Data.DateTime                        (DateTime)
import           Data.Map                             (Map)
import           Data.Set                             (Set)
import           Data.UUID.Types                      (UUID)
import           Data.Word                            (Word8)
import           Empire.API.Data.Node                 (NodeId)
import           Empire.API.Graph.CollaborationUpdate (ClientId)
import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Batch.Workspace
import           Luna.Studio.Event.Event              (Event)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App          (App)
import           Luna.Studio.React.Store              (Ref)
import           Luna.Studio.State.Action             (ActionRep, Connect, SomeAction)
import qualified Luna.Studio.State.Collaboration      as Collaboration
import qualified Luna.Studio.State.UI                 as UI
import           System.Random                        (StdGen)
import qualified System.Random                        as Random

-- TODO: Reconsider our design. @wdanilo says that we shouldn't use MonadState at all
data State = State
        { _ui                   :: UI.State
        , _backend              :: BackendState
        , _actions              :: ActionState
        , _collaboration        :: Collaboration.State
        , _debug                :: DebugState
        , _selectionHistory     :: [Set NodeId]
        -- TODO[react]: wyjebawszy
        , _workspace            :: Workspace
        , _lastEventTimestamp   :: DateTime
        , _random               :: StdGen
        }

data ActionState = ActionState
        { _currentActions       :: Map ActionRep (SomeAction (Command State))
        -- TODO[LJK]: This is duplicate. Find way to remove it but make it possible to get Connect without importing its instance
        , _currentConnectAction :: Maybe Connect
        } deriving (Default, Generic)

data BackendState = BackendState
        { _pendingRequests      :: Set UUID
        , _clientId             :: ClientId
        }

data DebugState = DebugState
        { _lastEvent            :: Maybe Event
        , _eventNum             :: Int
        } deriving (Default, Generic)

makeLenses ''ActionState
makeLenses ''BackendState
makeLenses ''State
makeLenses ''DebugState

mkState :: Ref App -> ClientId -> DateTime -> StdGen -> State
mkState ref clientId' = State
    {- react                -} (UI.mkState ref)
    {- backend              -} (BackendState def clientId')
    {- actions              -} def
    {- collaboration        -} def
    {- debug                -} def
    {- selectionHistory     -} def
    {- workspace            -} def

nextRandom :: Command State Word8
nextRandom = uses random Random.random >>= \(val, rnd) -> random .= rnd >> return val
