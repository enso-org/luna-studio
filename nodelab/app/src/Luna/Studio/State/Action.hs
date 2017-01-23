{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE ExistentialQuantification #-}
module Luna.Studio.State.Action where

import           Control.DeepSeq         (NFData)
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Dynamic
import           Data.Map                (Map)
import           Data.Position           (Position, ScreenPosition)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Empire.API.Data.Node    (NodeId)
import           Empire.API.Data.PortRef (AnyPortRef)
import           Luna.Studio.Prelude


data Drag = Drag { _dragStartPos :: Position
                 , _dragNodeId :: NodeId
                 , _dragNodesStartPos :: Map NodeId (Position)
                 } deriving (Eq, Show, Generic, Typeable)

makeLenses ''Drag
instance ToJSON Drag

data MultiSelection = MultiSelection { _multiSelecectionStartPos   :: Position
                                     } deriving (Eq, Show, Generic, Typeable)

makeLenses ''MultiSelection
instance ToJSON MultiSelection

data PanDrag = PanDrag { _panDragPreviousPos :: ScreenPosition
                       } deriving (Eq, Show, Generic, Typeable)

makeLenses ''PanDrag
instance ToJSON PanDrag

data ZoomDrag = ZoomDrag { _zoomDragFixedPoint  :: Position
                         , _zoomDragPreviousPos :: ScreenPosition
                         } deriving (Eq, Show, Generic, Typeable)

makeLenses ''ZoomDrag
instance ToJSON ZoomDrag


data SliderDrag = SliderDrag { _sliderDragPortRef   :: AnyPortRef
                             , _sliderDragStartPos  :: Position
                             , _sliderDragInitValue :: InitValue
                             } deriving (Eq, Show, Generic, Typeable)

data InitValue = Discrete  Int
              | Continous Double
              deriving (Eq, Show, Generic, NFData, Typeable)


makeLenses ''InitValue
makeLenses ''SliderDrag

instance ToJSON SliderDrag
instance ToJSON InitValue
instance FromJSON InitValue

data PenConnect = PenConnect { _penConnectHistory         :: [Position]
                             , _penConnectLastVisitedNode :: Maybe NodeId
                             } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PenConnect
instance ToJSON PenConnect

data PenDisconnect = PenDisconnect { _penDisconnectHistory         :: [Position]
                                   , _penDisconnectLastVisitedNode :: Maybe NodeId
                                   } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PenDisconnect
instance ToJSON PenDisconnect

data DragConnect = DragConnect { _dragConnectStartPos :: ScreenPosition
                               } deriving (Eq, Generic, Show, Typeable)

makeLenses ''DragConnect
instance ToJSON DragConnect

data ClickConnect = ClickConnect deriving (Eq, Generic, Show, Typeable)

makeLenses ''ClickConnect
instance ToJSON ClickConnect


data SomeAction m = forall a. (Action m a, Show a) => SomeAction Dynamic a deriving (Typeable)

instance Show (SomeAction m) where
    show (SomeAction _ a) = show a


class Monad m => Action m a where
    begin :: a -> m ()
    continue :: (a -> m ()) -> m ()
    update :: a -> m ()
    end :: a -> m ()

instance Monad m => Action m (SomeAction m) where
    begin  (SomeAction _ a) = begin a
    continue _              = return ()
    update (SomeAction _ a) = update a
    end    (SomeAction _ a) = end a

someAction :: (Show a, Action m a, Typeable a) => a -> SomeAction m
someAction a = SomeAction (toDyn a) a

fromSomeAction :: Typeable a => SomeAction m -> Maybe a
fromSomeAction (SomeAction d _) = fromDynamic d


newtype ActionRep = ActionRep TypeRep deriving (Show, Eq, Ord)

dragAction, multiSelectionAction, panDragAction, zoomDragAction, sliderDragAction, penConnectAction, penDisconnectAction, dragConnectAction, clickConnectAction :: ActionRep
dragAction           = ActionRep (typeOf Drag)
multiSelectionAction = ActionRep (typeOf MultiSelection)
panDragAction        = ActionRep (typeOf PanDrag)
zoomDragAction       = ActionRep (typeOf ZoomDrag)
sliderDragAction     = ActionRep (typeOf SliderDrag)
penConnectAction     = ActionRep (typeOf PenConnect)
penDisconnectAction  = ActionRep (typeOf PenDisconnect)
dragConnectAction    = ActionRep (typeOf DragConnect)
clickConnectAction   = ActionRep (typeOf ClickConnect)

overlappingActions :: [Set ActionRep]
overlappingActions = [ Set.fromList [ dragAction
                                    , multiSelectionAction
                                    , sliderDragAction
                                    , penConnectAction
                                    , penDisconnectAction
                                    , dragConnectAction
                                    , clickConnectAction
                                    ]
                     , Set.fromList [ panDragAction
                                    , zoomDragAction
                                    ]
                     ]






-- type family StateOf a
--
-- data Drag
-- data MultiSelection
-- data PanDrag
-- data ZoomDrag
-- data Slider
-- data PenConnect
-- data PenDisconnect
-- data Connect
--
-- connect = typeOf Connect
-- multiSelection = typeOf MultiSelection
--
-- type instance StateOf Drag           = Drag.State
-- type instance StateOf MultiSelection = MultiSelection.State
-- type instance StateOf PanDrag        = PanDrag.State
-- type instance StateOf ZoomDrag       = ZoomDrag.State
-- type instance StateOf Slider         = Slider.State
-- type instance StateOf PenConnect     = PenConnect.State
-- type instance StateOf PenDisconnect  = PenDisconnect.State
-- type instance StateOf Connect        = Connect.State
--
--
-- newtype Action a = Action (StateOf a)
-- makeWrapped ''Action
--
-- newtype SomeAction = SomeAction Dynamic (Command State ())
--
-- someAction :: Action a -> SomeAction
-- someAction a = SomeAction (toDynamic a) (exit $ unwrap' a)
--
-- fromSomeAction :: SomeAction -> Maybe (Action a)
-- fromSomeAction (SomeAction d _) = fromDynamic d
--
-- data GlobalState = GlobalState { _currentActions :: Map TypeRep SomeAction
--                                }
--
--
-- checkSomeAction :: forall a. GlobalState -> Maybe SomeAction
-- checkSomeAction s = Map.lookup (typeRep (Proxy :: Proxy a)) (s ^. currentAction)
--
-- checkAction :: GlobalState -> Maybe (Action a)
-- checkAction s = join $ fromSomeAction <$> checkSomeAction s
--
-- checkIfActionPerfoming :: GlobalState -> a -> Bool
-- checkIfActionPerfoming s a = Map.member (typeOf a) (s ^. currentAction)
--
-- -- checkIfActionPerfoming Drag
--
--
-- ------------- Grupy akcji wykluczajacych sie
--
-- noOverlappingActions :: [Set TypeRep]
-- noOverlappingActions = [Set.fromList [connect, multiSelect]]
--
-- runningActions :: GlobalState -> [TypeRep]
-- runningActions = ...
--
-- czymozemyAkcje :: GlobalState -> a ->
-- czymozemyAkcje s a = (typeOf a,) <$> all ... where
--     all = runningActions s
--
--
--
-- 1) do osobnego pliku
-- 2) ten plik kompiluje sie
-- 3) do global state nowe pole
-- ...
