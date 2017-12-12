{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.State.Global where

import           Common.Action.Command                    (Command)
import           Common.Debug                             (HasRequestTimes, requestTimes)
import           Common.Prelude
import           Data.HashMap.Lazy                        (HashMap)
import           Data.Map                                 (Map)
import           Data.Set                                 (Set)
import           Data.Time.Clock                          (UTCTime)
import           Data.UUID.Types                          (UUID)
import           Data.Word                                (Word8)
import           LunaStudio.API.Graph.CollaborationUpdate (ClientId)
import           LunaStudio.Data.NodeLoc                  (NodeLoc)
import           LunaStudio.Data.NodeSearcher             (NodeSearcherData)
import           LunaStudio.Data.NodeValue                (Visualizer, VisualizerMatcher, VisualizerName)
import           LunaStudio.Data.TypeRep                  (TypeRep)
import           NodeEditor.Event.Event                   (Event)
import           NodeEditor.React.Model.App               (App)
import           NodeEditor.React.Store                   (Ref)
import           NodeEditor.State.Action                  (ActionRep, Connect, SomeAction)
import qualified NodeEditor.State.Collaboration           as Collaboration
import qualified NodeEditor.State.UI                      as UI
import           System.Random                            (StdGen)
import qualified System.Random                            as Random


-- TODO: Reconsider our design. @wdanilo says that we shouldn't use MonadState at all
data State = State
        { _ui                   :: UI.State
        , _backend              :: BackendState
        , _actions              :: ActionState
        , _collaboration        :: Collaboration.State
        , _debug                :: DebugState
        , _selectionHistory     :: [Set NodeLoc]
        , _nodeSearcherData     :: NodeSearcherData
        , _waitingForTc         :: Bool
        , _preferedVisualizers  :: HashMap TypeRep Visualizer
        , _visualizers          :: Map VisualizerName VisualizerMatcher
        , _random               :: StdGen
        }

data ActionState = ActionState
        { _currentActions       :: Map ActionRep (SomeAction (Command State))
        -- TODO[LJK]: This is duplicate. Find way to remove it but make it possible to get Connect without importing its instance
        , _currentConnectAction :: Maybe Connect
        } deriving (Default, Generic)

data BackendState = BackendState
        { _pendingRequests      :: Map UUID UTCTime
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

mkState :: Ref App -> ClientId -> HashMap TypeRep Visualizer -> Map VisualizerName VisualizerMatcher -> StdGen -> State
mkState ref clientId' = State
    {- react                -} (UI.mkState ref)
    {- backend              -} (BackendState def clientId')
    {- actions              -} def
    {- collaboration        -} def
    {- debug                -} def
    {- selectionHistory     -} def
    {- nodeSearcherData     -} def
    {- waitingForTc         -} False

nextRandom :: Command State Word8
nextRandom = uses random Random.random >>= \(val, rnd) -> random .= rnd >> return val

instance HasRequestTimes State where
    requestTimes = backend . pendingRequests
