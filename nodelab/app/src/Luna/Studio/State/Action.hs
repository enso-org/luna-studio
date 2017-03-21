{-# LANGUAGE ExistentialQuantification #-}
module Luna.Studio.State.Action where

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Curve                   (Curve)
import           Data.Dynamic
import           Data.Map                     (Map)
import           Data.Position                (Position)
import           Data.ScreenPosition          (ScreenPosition)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Empire.API.Data.Connection   (ConnectionId)
import           Empire.API.Data.Port         (PortId)
import           Empire.API.Data.PortRef      (AnyPortRef)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node (Node, NodeId)


data NodeDrag = NodeDrag { _nodeDragStartPos      :: Position
                         , _nodeDragNodeId        :: NodeId
                         , _nodeDragNodesStartPos :: Map NodeId Position
                         , _nodeDragSnappedConn   :: Maybe ConnectionId
                         } deriving (Eq, Show, Generic, Typeable)

makeLenses ''NodeDrag

data MultiSelection = MultiSelection { _multiSelecectionStartPos :: Position
                                     } deriving (Eq, Show, Generic, Typeable)

makeLenses ''MultiSelection

data PanDrag = PanDrag { _panDragPreviousPos :: ScreenPosition
                       } deriving (Eq, Show, Generic, Typeable)

makeLenses ''PanDrag

data ZoomDrag = ZoomDrag { _zoomDragFixedPoint  :: ScreenPosition
                         , _zoomDragPreviousPos :: ScreenPosition
                         } deriving (Eq, Show, Generic, Typeable)

makeLenses ''ZoomDrag


data SliderDrag = SliderDrag { _sliderDragPortRef   :: AnyPortRef
                             , _sliderDragStartPos  :: ScreenPosition
                             , _sliderDragInitValue :: InitValue
                             } deriving (Eq, Show, Generic, Typeable)

data InitValue = Discrete  Int
               | Continous Double
               deriving (Eq, Show, Generic, Typeable)

instance NFData InitValue

makeLenses ''InitValue
makeLenses ''SliderDrag

instance ToJSON InitValue
instance FromJSON InitValue

data PenConnect = PenConnect { _penConnectCurve           :: Curve
                             , _penConnectLastVisitedNode :: Maybe NodeId
                             } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PenConnect

data PenDisconnect = PenDisconnect { _penDisconnectCurve                :: Curve
                                   , _penDisconnectLastVisitedNode      :: Maybe NodeId
                                   , _penDisconnectNextNodeRestriction :: Maybe NodeId
                                   } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PenDisconnect

data Mode = Drag | Click deriving (Eq, Generic, Show, Typeable)

data Connect = Connect { _connectStartPos       :: ScreenPosition
                       , _connectSourcePort     :: AnyPortRef
                       , _connectIsConnModified :: Bool
                       , _connectSnappedPort    :: Maybe AnyPortRef
                       , _connectMode           :: Mode
                       } deriving (Eq, Generic, Show, Typeable)

makeLenses ''Connect

data PortDrag = PortDrag { _portDragStartPos     :: ScreenPosition
                         , _portDragPortRef      :: AnyPortRef
                         , _portDragPortMapping  :: Map PortId PortId
                         , _portDragMode         :: Mode
                         , _portDragOriginalNode :: Node
                         } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PortDrag

data Searcher = Searcher deriving (Eq, Generic, Show, Typeable)

makeLenses ''Searcher

data VisualizationDrag = VisualizationDrag
    { _visNodeId :: NodeId
    , _visIdx    :: Int
    , _visPos    :: Position
    } deriving (Eq, Show, Generic, Typeable)

makeLenses ''VisualizationDrag

data SomeAction m = forall a. (Action m a, Show a, Typeable a) => SomeAction Dynamic a deriving (Typeable)

instance Show (SomeAction m) where
    show (SomeAction _ a) = show a


class (Typeable a, Show a, Monad m) => Action m a where
    begin :: a -> m ()
    continue :: (a -> m ()) -> m ()
    update :: a -> m ()
    end :: a -> m ()

instance (Typeable m, Monad m) => Action m (SomeAction m) where
    begin  (SomeAction _ a) = begin a
    continue _              = return ()
    update (SomeAction _ a) = update a
    end    (SomeAction _ a) = end a

someAction :: Action m a => a -> SomeAction m
someAction a = SomeAction (toDyn a) a

fromSomeAction :: Typeable a => SomeAction m -> Maybe a
fromSomeAction (SomeAction d _) = fromDynamic d


newtype ActionRep = ActionRep TypeRep deriving (Show, Eq, Ord)

nodeDragAction, multiSelectionAction, visualizationDragAction, panDragAction, zoomDragAction, sliderDragAction, penConnectAction, penDisconnectAction, connectAction, portDragAction, searcherAction :: ActionRep
nodeDragAction          = ActionRep (typeOf NodeDrag)
multiSelectionAction    = ActionRep (typeOf MultiSelection)
panDragAction           = ActionRep (typeOf PanDrag)
zoomDragAction          = ActionRep (typeOf ZoomDrag)
sliderDragAction        = ActionRep (typeOf SliderDrag)
penConnectAction        = ActionRep (typeOf PenConnect)
penDisconnectAction     = ActionRep (typeOf PenDisconnect)
connectAction           = ActionRep (typeOf Connect)
portDragAction          = ActionRep (typeOf PortDrag)
searcherAction          = ActionRep (typeOf Searcher)
visualizationDragAction = ActionRep (typeOf VisualizationDrag)

overlappingActions :: [Set ActionRep]
overlappingActions = [ Set.fromList [ connectAction
                                    , multiSelectionAction
                                    , nodeDragAction
                                    , penConnectAction
                                    , penDisconnectAction
                                    , searcherAction
                                    , sliderDragAction
                                    , visualizationDragAction
                                    , portDragAction
                                    ]
                     , Set.fromList [ panDragAction
                                    , zoomDragAction
                                    , portDragAction
                                    ]
                     ]

actionsBlockingPortHighlight :: Set ActionRep
actionsBlockingPortHighlight = Set.fromList [ multiSelectionAction
                                            , nodeDragAction
                                            , penConnectAction
                                            , penDisconnectAction
                                            , searcherAction
                                            , sliderDragAction
                                            , visualizationDragAction
                                            , panDragAction
                                            , zoomDragAction
                                            , portDragAction
                                            ]
