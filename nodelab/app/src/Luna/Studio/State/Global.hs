{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Studio.State.Global where

import           Data.Word                            (Word8)
import           Luna.Studio.Data.Vector              (Position (Position), Vector2 (Vector2))
import           Luna.Studio.Prelude

import qualified Control.Monad.State                  as M
import           Data.Aeson                           (ToJSON, toJSON)
import           Data.DateTime                        (DateTime)
import           Data.Set                             (Set)
import           Data.UUID.Types                      (UUID)
import           Luna.Studio.Action.Command           (Command)
import           System.Random                        (StdGen)
import qualified System.Random                        as Random

import           Empire.API.Data.Connection           (ConnectionId)
import           Empire.API.Data.Node                 (NodeId)
import qualified Empire.API.Data.Node                 as Node
import qualified Empire.API.Graph.Collaboration       as Collaboration
import qualified Event.Event                          as Event
import           Luna.Studio.Batch.Workspace
import           Luna.Studio.React.Model.App          (App)
import qualified Luna.Studio.React.Model.App          as App
import           Luna.Studio.React.Model.Breadcrumbs  (Breadcrumbs)
import           Luna.Studio.React.Model.CodeEditor   (CodeEditor)
import           Luna.Studio.React.Model.Connection   (Connection, CurrentConnection)
import           Luna.Studio.React.Model.Node         (Node)
import           Luna.Studio.React.Model.NodeEditor   (NodeEditor)
import qualified Luna.Studio.React.Model.NodeEditor   as NodeEditor
import           Luna.Studio.React.Model.Searcher     (Searcher)
import           Luna.Studio.React.Model.SelectionBox (SelectionBox)
import           Luna.Studio.React.Store              (Ref)
import qualified Luna.Studio.React.Store              as Store
import           Luna.Studio.State.Action             (Action)
import qualified Luna.Studio.State.Collaboration      as Collaboration
import qualified Luna.Studio.State.Graph              as Graph



foreign import javascript safe "{}" defJsState :: Event.JSState

-- TODO[react]: Move all action states to ActionState
-- TODO split to more states
data State = State { _mousePos           :: Position
                   , _graph              :: Graph.State
                   , _renderNeeded       :: Bool --TODO refactor
                   , _performedAction    :: Maybe Action
                   , _selectionHistory   :: [Set Node.NodeId]
                   -- TODO[react]: wyjebawszy
                   , _workspace          :: Workspace
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
    toJSON _ = toJSON "(Ref App)"


makeLenses ''State


withApp :: (Ref App -> Command State r) -> Command State r
withApp action = action =<< use app

modify lens action = do
    renderNeeded .= True
    withApp $ Store.continueModify $ zoom lens action

get lens = withApp $ return . view lens <=< Store.get

modifyApp :: M.State App r -> Command State r
modifyApp action = do
    renderNeeded .= True
    withApp $ Store.continueModify action

renderIfNeeded :: Command State ()
renderIfNeeded =
    whenM (use renderNeeded) $ do
        withApp Store.commit
        renderNeeded .= False

modifyNodeEditor :: M.State NodeEditor r -> Command State r
modifyNodeEditor = modify App.nodeEditor

getNodeEditor :: Command State NodeEditor
getNodeEditor = get App.nodeEditor

modifyCodeEditor :: M.State CodeEditor r -> Command State r
modifyCodeEditor = modify App.codeEditor

modifyBreadcrumbs :: M.State Breadcrumbs r -> Command State r
modifyBreadcrumbs = modify App.breadcrumbs

modifySearcher :: M.State Searcher r -> Command State r
modifySearcher = modify App.searcher

getSearcher :: Command State Searcher
getSearcher = get App.searcher

modifySelectionBox :: M.State SelectionBox r -> Command State r
modifySelectionBox = modify (App.nodeEditor . NodeEditor.selectionBox)

modifyCurrentConnection :: Monoid r => M.State CurrentConnection r -> Command State r
modifyCurrentConnection = modify (App.nodeEditor . NodeEditor.currentConnection) . zoom traverse

modifyNode :: Monoid r => NodeId -> M.State Node r -> Command State r
modifyNode nodeId = modify (App.nodeEditor . NodeEditor.nodes . at nodeId) . zoom traverse

getNode :: NodeId -> Command State (Maybe Node)
getNode nodeId = get (App.nodeEditor . NodeEditor.nodes . at nodeId)

modifyConnection :: Monoid r => ConnectionId -> M.State Connection r -> Command State r
modifyConnection connectionId = modify (App.nodeEditor . NodeEditor.connections . at connectionId) . zoom traverse

getConnection :: ConnectionId -> Command State (Maybe Connection)
getConnection connectionId = get (App.nodeEditor . NodeEditor.connections . at connectionId)

initialState :: DateTime -> Collaboration.ClientId -> StdGen -> Maybe Int -> Ref App -> State
initialState = State (Position (Vector2 200 200)) def False def def def def def defJsState def def

nextRandom :: Command State Word8
nextRandom = do
    (val, rnd) <- uses random Random.random
    random .= rnd
    return val
