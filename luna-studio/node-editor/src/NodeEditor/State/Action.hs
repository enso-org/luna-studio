{-# LANGUAGE ExistentialQuantification #-}
module NodeEditor.State.Action where

import           Common.Prelude
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.Curve                           (Curve)
import           Data.Dynamic
import           Data.Map                             (Map)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Time.Clock                      (UTCTime)
import           LunaStudio.Data.NodeLoc              (NodeLoc)
import           LunaStudio.Data.PortRef              (AnyPortRef, InPortRef, OutPortRef)
import           LunaStudio.Data.Position             (Position)
import           LunaStudio.Data.ScreenPosition       (ScreenPosition)
import           NodeEditor.Data.Slider               (InitValue)
import           NodeEditor.React.Model.Connection    (ConnectionId)
import           NodeEditor.React.Model.Visualization (VisualizationId, VisualizationMode)

data NodeDrag = NodeDrag { _nodeDragStartPos      :: Position
                         , _nodeDragNodeLoc       :: NodeLoc
                         , _nodeDragNodesStartPos :: Map NodeLoc Position
                         , _nodeDragSnappedConnId :: Maybe ConnectionId
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


data SliderDrag = SliderDrag { _sliderDragPortRef   :: InPortRef
                             , _sliderDragStartTime :: UTCTime
                             , _sliderDragInitValue :: InitValue
                             } deriving (Eq, Show, Generic, Typeable)

makeLenses ''SliderDrag

data PenConnect = PenConnect { _penConnectCurve           :: Curve
                             , _penConnectLastVisitedNode :: Maybe NodeLoc
                             } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PenConnect

data PenDisconnect = PenDisconnect { _penDisconnectCurve               :: Curve
                                   , _penDisconnectLastVisitedNode     :: Maybe NodeLoc
                                   , _penDisconnectNextNodeRestriction :: Maybe NodeLoc
                                   } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PenDisconnect

data Mode = Drag | Click deriving (Eq, Generic, Show, Typeable)

data Connect = Connect { _connectStartPos              :: ScreenPosition
                       , _connectSourcePort            :: AnyPortRef
                       , _connectIsConnModified        :: Bool
                       , _connectSnappedPort           :: Maybe AnyPortRef
                       , _connectIsArgumentConstructor :: Bool
                       , _connectMode                  :: Mode
                       } deriving (Eq, Generic, Show, Typeable)

makeLenses ''Connect

data PortDrag = PortDrag { _portDragStartPos              :: ScreenPosition
                         , _portDragPortStartPosInSidebar :: Position
                         , _portDragStartPortRef          :: OutPortRef
                         , _portDragActPortRef            :: OutPortRef
                         , _portDragIsArgumentConstructor :: Bool
                         , _portDragMode                  :: Mode
                         } deriving (Eq, Generic, Show, Typeable)

makeLenses ''PortDrag

data Searcher = Searcher deriving (Eq, Generic, Show, Typeable)

makeLenses ''Searcher

data VisualizationDrag = VisualizationDrag { _visNodeLoc :: NodeLoc
                                           , _visIdx     :: Int
                                           , _visPos     :: Position
                                           } deriving (Eq, Show, Generic, Typeable)

makeLenses ''VisualizationDrag


data VisualizationActive = VisualizationActive { _visualizationActiveNodeLoc         :: NodeLoc
                                               , _visualizationActiveVisualizationId :: VisualizationId
                                               , _visualizationActiveSelectedMode    :: VisualizationMode
                                               , _visualizationActiveTriggeredByVis  :: Bool
                                               } deriving (Eq, Show, Generic, Typeable)

makeLenses ''VisualizationActive

data DocVisualizationActive = DocVisualizationActive { _docVisualizationActiveSelectedMode    :: VisualizationMode
                                                     , _docVisualizationActiveTriggeredByVis  :: Bool
                                                     } deriving (Eq, Show, Generic, Typeable)

makeLenses ''DocVisualizationActive


data SidebarAddRemoveMode = SidebarAddRemoveMode { _sidebarAddRemoveModeNodeLoc :: NodeLoc
                                                 } deriving (Eq, Show, Generic, Typeable)

makeLenses ''SidebarAddRemoveMode

data TextPortControlEdit = TextPortControlEdit { _textPortControlEditPortRef :: InPortRef
                                               , _textPortControlEditValue   :: Text
                                               } deriving (Eq, Show, Generic, Typeable)

makeLenses ''TextPortControlEdit

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

nodeDragAction, multiSelectionAction, visualizationDragAction, panDragAction, zoomDragAction, sidebarAddRemoveModeAction, sliderDragAction, penConnectAction, penDisconnectAction, connectAction, portDragAction, searcherAction, textPortControlEditAction, visualizationActiveAction, docVisualizationActiveAction :: ActionRep
docVisualizationActiveAction = ActionRep (typeOf DocVisualizationActive)
nodeDragAction               = ActionRep (typeOf NodeDrag)
multiSelectionAction         = ActionRep (typeOf MultiSelection)
panDragAction                = ActionRep (typeOf PanDrag)
zoomDragAction               = ActionRep (typeOf ZoomDrag)
sliderDragAction             = ActionRep (typeOf SliderDrag)
penConnectAction             = ActionRep (typeOf PenConnect)
penDisconnectAction          = ActionRep (typeOf PenDisconnect)
connectAction                = ActionRep (typeOf Connect)
portDragAction               = ActionRep (typeOf PortDrag)
sidebarAddRemoveModeAction   = ActionRep (typeOf SidebarAddRemoveMode)
searcherAction               = ActionRep (typeOf Searcher)
textPortControlEditAction    = ActionRep (typeOf TextPortControlEdit)
visualizationDragAction      = ActionRep (typeOf VisualizationDrag)
visualizationActiveAction    = ActionRep (typeOf VisualizationActive)

overlappingActions :: [Set ActionRep]
overlappingActions = [ Set.fromList [ connectAction
                                    , multiSelectionAction
                                    , nodeDragAction
                                    , penConnectAction
                                    , penDisconnectAction
                                    , searcherAction
                                    , sidebarAddRemoveModeAction
                                    , sliderDragAction
                                    , visualizationDragAction
                                    , portDragAction
                                    , visualizationActiveAction
                                    , textPortControlEditAction
                                    ]
                     , Set.fromList [ connectAction
                                    , multiSelectionAction
                                    , nodeDragAction
                                    , penConnectAction
                                    , penDisconnectAction
                                    , sidebarAddRemoveModeAction
                                    , sliderDragAction
                                    , visualizationDragAction
                                    , portDragAction
                                    , visualizationActiveAction
                                    , docVisualizationActiveAction
                                    ]
                     , Set.fromList [ panDragAction
                                    , zoomDragAction
                                    , portDragAction
                                    , visualizationActiveAction
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

actionsClosingOnMouseLeave :: Set ActionRep
actionsClosingOnMouseLeave = Set.fromList [ nodeDragAction
                                          , multiSelectionAction
                                          , panDragAction
                                          , zoomDragAction
                                          , sliderDragAction
                                          , penConnectAction
                                          , penDisconnectAction
                                          , portDragAction
                                          , visualizationDragAction
                                          , docVisualizationActiveAction
                                          ]
