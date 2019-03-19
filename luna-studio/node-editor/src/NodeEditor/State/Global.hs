module NodeEditor.State.Global where

import Common.Prelude

import qualified NodeEditor.React.Model.App                as App
import qualified NodeEditor.React.Model.Searcher.Hint.Node as Searcher
import qualified NodeEditor.React.Store.Ref                as Ref
import qualified NodeEditor.State.Collaboration            as Collaboration
import qualified NodeEditor.State.UI                       as UI
import qualified System.Random                             as Random

import Common.Action.Command                    (Command)
import Data.HashMap.Lazy                        (HashMap)
import Data.Map                                 (Map)
import Data.Set                                 (Set)
import Data.Time.Clock                          (UTCTime)
import Data.UUID.Types                          (UUID)
import Data.Word                                (Word8)
import Luna.Benchmark                           (HasRequestTimes, requestTimes)
import LunaStudio.API.Graph.CollaborationUpdate (ClientId)
import LunaStudio.Data.NodeLoc                  (NodeLoc)
import LunaStudio.Data.TypeRep                  (TypeRep)
import NodeEditor.Event.Event                   (Event)
import NodeEditor.React.Model.App               (App)
import NodeEditor.React.Model.NodeEditor        (NodeEditor)
import NodeEditor.React.Model.Visualization     (Visualizer, VisualizerId,
                                                 VisualizerMatcher,
                                                 VisualizerPath)
import NodeEditor.React.Store                   (Ref)
import NodeEditor.State.Action                  (ActionRep, Connect, SomeAction)
import System.Random                            (StdGen)


-- TODO: Reconsider our design. @wdanilo says that we shouldn't use MonadState at all
data State = State
        { _ui                  :: UI.State
        , _backend             :: BackendState
        , _actions             :: ActionState
        , _collaboration       :: Collaboration.State
        , _debug               :: DebugState
        , _selectionHistory    :: [Set NodeLoc]
        , _searcherDatabase    :: Searcher.Database
        , _preferedVisualizers :: HashMap TypeRep Visualizer
        , _visualizers         :: Map VisualizerId VisualizerMatcher
        , _internalVisualizers :: Map VisualizerId VisualizerPath
        , _random              :: StdGen
        }

data ActionState = ActionState
        { _currentActions       :: Map ActionRep (SomeAction (Command State))
        -- TODO[LJK]: This is duplicate. Find way to remove it but make it possible to get Connect without importing its instance
        , _currentConnectAction :: Maybe Connect
        } deriving (Generic)

instance Default ActionState

data BackendState = BackendState
        { _pendingRequests :: Map UUID UTCTime
        , _clientId        :: ClientId
        }

data DebugState = DebugState
        { _lastEvent :: Maybe Event
        , _eventNum  :: Int
        } deriving (Generic)

instance Default DebugState

makeLenses ''ActionState
makeLenses ''BackendState
makeLenses ''State
makeLenses ''DebugState

mkState :: Ref App -> ClientId -> StdGen -> State
mkState ref clientId' = State
    {- react               -} (UI.mkState ref)
    {- backend             -} (BackendState def clientId')
    {- actions             -} def
    {- collaboration       -} def
    {- debug               -} def
    {- selectionHistory    -} def
    {- searcherDatabase    -} def
    {- preferedVisualizers -} mempty
    {- visualizers         -} mempty
    {- internalVisualizers -} mempty


nextRandom :: Command State Word8
nextRandom = uses random Random.random >>= \(val, rnd) -> random .= rnd >> return val

getNodeEditor :: MonadIO m => State -> m NodeEditor
getNodeEditor state = view App.nodeEditor <$> Ref.get (state ^. ui . UI.app)

instance HasRequestTimes State where
    requestTimes = backend . pendingRequests
