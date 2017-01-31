{-# LANGUAGE ExistentialQuantification #-}
module Luna.Studio.State.Action where

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Dynamic
import           Data.Map                (Map)
import           Data.Position           (Position, ScreenPosition)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Empire.API.Data.Node    (NodeId)
import           Empire.API.Data.PortRef (AnyPortRef)
import           Luna.Studio.Prelude


data NodeDrag = NodeDrag { _nodeDragStartPos      :: Position
                         , _nodeDragNodeId        :: NodeId
                         , _nodeDragNodesStartPos :: Map NodeId (Position)
                         } deriving (Eq, Show, Generic, Typeable)

makeLenses ''NodeDrag
instance ToJSON NodeDrag

data MultiSelection = MultiSelection { _multiSelecectionStartPos :: Position
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
               deriving (Eq, Show, Generic, Typeable)

instance NFData InitValue

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

data Searcher = Searcher deriving (Eq, Generic, Show, Typeable)

makeLenses ''Searcher
instance ToJSON Searcher


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

nodeDragAction, multiSelectionAction, panDragAction, zoomDragAction, sliderDragAction, penConnectAction, penDisconnectAction, dragConnectAction, clickConnectAction, searcherAction :: ActionRep
nodeDragAction       = ActionRep (typeOf NodeDrag)
multiSelectionAction = ActionRep (typeOf MultiSelection)
panDragAction        = ActionRep (typeOf PanDrag)
zoomDragAction       = ActionRep (typeOf ZoomDrag)
sliderDragAction     = ActionRep (typeOf SliderDrag)
penConnectAction     = ActionRep (typeOf PenConnect)
penDisconnectAction  = ActionRep (typeOf PenDisconnect)
dragConnectAction    = ActionRep (typeOf DragConnect)
clickConnectAction   = ActionRep (typeOf ClickConnect)
searcherAction       = ActionRep (typeOf Searcher)

overlappingActions :: [Set ActionRep]
overlappingActions = [ Set.fromList [ nodeDragAction
                                    , multiSelectionAction
                                    , sliderDragAction
                                    , penConnectAction
                                    , penDisconnectAction
                                    , dragConnectAction
                                    , clickConnectAction
                                    , searcherAction
                                    ]
                     , Set.fromList [ panDragAction
                                    , zoomDragAction
                                    ]
                     ]
