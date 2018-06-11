module NodeEditor.Event.View where

import           Common.Analytics                   (IsTrackedEvent (isTracked))
import           Common.Data.Event                  (EventName (eventName))
import           Common.Prelude
import           Control.Lens.Aeson                 (parseDropUnary, toEncodingDropUnary)
import           Data.Aeson                         (FromJSON (..), ToJSON (..))
import           Data.Convert                       (Convertible (convert))
import           LunaStudio.Data.NodeLoc            (NodeLoc)
import           LunaStudio.Data.PortRef            (InPortRef (InPortRef), OutPortRef (OutPortRef))
import           LunaStudio.Data.ScreenPosition     (ScreenPosition, fromDoubles)
import           LunaStudio.Data.Visualization      (VisualizationId)
import           LunaStudio.Data.Visualizer         (VisualizerId (VisualizerId))
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumb, BreadcrumbItem)
import           Prelude                            (error)


type Path = [String]

newtype Target = Target { unTarget :: [String] }
    deriving (Generic, Show)

data MouseEvent = MouseEvent
    { _altKey           :: Bool
    , _bubbles          :: Bool
    , _button           :: Int
    , _buttons          :: Double
    , _cancelable       :: Bool
    , _cancelBubble     :: Bool
    , _clientX          :: Double
    , _clientY          :: Double
    , _ctrlKey          :: Bool
    , _defaultPrevented :: Bool
    , _detail           :: Double
    , _eventPhase       :: Double
    , _isTrusted        :: Bool
    , _layerX           :: Double
    , _layerY           :: Double
    , _metaKey          :: Bool
    , _movementY        :: Double
    , _pageX            :: Double
    , _pageY            :: Double
    , _screenX          :: Double
    , _shiftKey         :: Bool
    , _screenY          :: Double
    , _movementX        :: Double
    , _timeStamp        :: Double
    , _offsetX          :: Double
    , _offsetY          :: Double
    , _returnValue      :: Bool
    , _type_            :: String
    , _which            :: Double
    , _x                :: Double
    , _y                :: Double
    } deriving (Generic, Show)

makeLenses ''MouseEvent

data NavigateEvent = NavigateEvent
    { _to :: Breadcrumb BreadcrumbItem
    , _aesonTooOld1 :: Maybe () --FIXME: replace with tagSingleConstructors when Aeson bumped
    } deriving (Generic, Show)

makeLenses ''NavigateEvent

data NodeMoveEvent = NodeMoveEvent
    { _position :: (Double, Double)
    , _aesonTooOld2 :: Maybe () --FIXME: replace with tagSingleConstructors when Aeson bumped
    } deriving (Generic, Show)

makeLenses ''NodeMoveEvent

data NodeSelectEvent = NodeSelectEvent
    { _select :: Bool
    , _aesonTooOld3 :: Maybe () --FIXME: replace with tagSingleConstructors when Aeson bumped
    } deriving (Generic, Show)

makeLenses ''NodeSelectEvent

data DisconnectEvent = DisconnectEvent
    { _src :: Bool
    , _aesonTooOld4 :: Maybe () --FIXME: replace with tagSingleConstructors when Aeson bumped
    } deriving (Generic, Show)

makeLenses ''DisconnectEvent

data SearcherAcceptEvent = SearcherAcceptEvent
    { _acceptSelectionStart :: Int
    , _acceptSelectionEnd   :: Int
    , _acceptValue          :: Text
    } deriving (Generic, Show)

makeLenses ''SearcherAcceptEvent

data SearcherEditEvent = SearcherEditEvent
    { _editSelectionStart :: Int
    , _editSelectionEnd   :: Int
    , _editValue          :: Text
    } deriving (Generic, Show)

makeLenses ''SearcherEditEvent

data SelectVisualizerEvent = SelectVisualizerEvent
    { _visualizerId :: VisualizerId
    , _aesonTooOld5 :: Maybe () --FIXME: replace with tagSingleConstructors when Aeson bumped
    } deriving (Generic, Show)

makeLenses ''SelectVisualizerEvent

data FocusVisualizationEvent = FocusVisualizationEvent
    { _aesonTooOld6 :: Maybe ()
    , _aesonTooOld7 :: Maybe ()
    } deriving (Generic, Show)

makeLenses ''FocusVisualizationEvent

data ToggleVisualizationsEvent = ToggleVisualizationsEvent
    { _aesonTooOld8 :: Maybe ()
    , _aesonTooOld9 :: Maybe ()
    } deriving (Generic, Show)

makeLenses ''ToggleVisualizationsEvent

data BaseEvent
    = Mouse                MouseEvent
    | Navigate             NavigateEvent
    | NodeMove             NodeMoveEvent
    | NodeSelect           NodeSelectEvent
    | Disconnect           DisconnectEvent
    | SearcherAccept       SearcherAcceptEvent
    | SearcherEdit         SearcherEditEvent
    | SelectVisualizer     SelectVisualizerEvent
    | FocusVisualization   FocusVisualizationEvent
    | ToggleVisualizations ToggleVisualizationsEvent
    deriving (Generic, Show)

makePrisms ''BaseEvent

data ViewEvent = ViewEvent
    { _path   :: Path
    , _target :: Target
    , _base   :: BaseEvent
    } deriving (Generic, Show)

makeLenses ''ViewEvent

instance NFData MouseEvent
instance NFData NavigateEvent
instance NFData NodeMoveEvent
instance NFData NodeSelectEvent
instance NFData DisconnectEvent
instance NFData SearcherAcceptEvent
instance NFData SearcherEditEvent
instance NFData SelectVisualizerEvent
instance NFData FocusVisualizationEvent
instance NFData ToggleVisualizationsEvent
instance NFData Target
instance NFData ViewEvent
instance NFData BaseEvent

instance FromJSON MouseEvent                where parseJSON = parseDropUnary
instance FromJSON NavigateEvent             where parseJSON = parseDropUnary
instance FromJSON NodeMoveEvent             where parseJSON = parseDropUnary
instance FromJSON NodeSelectEvent           where parseJSON = parseDropUnary
instance FromJSON DisconnectEvent           where parseJSON = parseDropUnary
instance FromJSON SearcherAcceptEvent       where parseJSON = parseDropUnary
instance FromJSON SearcherEditEvent         where parseJSON = parseDropUnary
instance FromJSON SelectVisualizerEvent     where parseJSON = parseDropUnary
instance FromJSON FocusVisualizationEvent   where parseJSON = parseDropUnary
instance FromJSON ToggleVisualizationsEvent where parseJSON = parseDropUnary
instance FromJSON Target                    where parseJSON = parseDropUnary
instance FromJSON ViewEvent                 where parseJSON = parseDropUnary
instance FromJSON BaseEvent                 where parseJSON = parseDropUnary


instance ToJSON MouseEvent                where toEncoding = toEncodingDropUnary
instance ToJSON NavigateEvent             where toEncoding = toEncodingDropUnary
instance ToJSON NodeMoveEvent             where toEncoding = toEncodingDropUnary
instance ToJSON NodeSelectEvent           where toEncoding = toEncodingDropUnary
instance ToJSON DisconnectEvent           where toEncoding = toEncodingDropUnary
instance ToJSON SearcherAcceptEvent       where toEncoding = toEncodingDropUnary
instance ToJSON SearcherEditEvent         where toEncoding = toEncodingDropUnary
instance ToJSON SelectVisualizerEvent     where toEncoding = toEncodingDropUnary
instance ToJSON FocusVisualizationEvent   where toEncoding = toEncodingDropUnary
instance ToJSON ToggleVisualizationsEvent where toEncoding = toEncodingDropUnary
instance ToJSON Target                    where toEncoding = toEncodingDropUnary
instance ToJSON ViewEvent                 where toEncoding = toEncodingDropUnary
instance ToJSON BaseEvent                 where toEncoding = toEncodingDropUnary

instance EventName ViewEvent where
    eventName = intercalate "." . view path
instance IsTrackedEvent ViewEvent where
    isTracked = const False

mouseAltKey :: BaseEvent -> Bool
mouseAltKey = \case
    Mouse evt -> evt ^. altKey
    _         -> False

mouseCtrlKey :: BaseEvent -> Bool
mouseCtrlKey = \case
    Mouse evt -> evt ^. ctrlKey
    _         -> False

mouseShiftKey :: BaseEvent -> Bool
mouseShiftKey = \case
    Mouse evt -> evt ^. shiftKey
    _         -> False

mouseMetaKey :: BaseEvent -> Bool
mouseMetaKey = \case
    Mouse evt -> evt ^. metaKey
    _         -> False

mouseButton ::  Int -> BaseEvent -> Bool
mouseButton btn = \case
    Mouse evt -> evt ^. button == btn
    _         -> False

leftButton :: Int
leftButton = 0

middleButton :: Int
middleButton = 1

rightButton :: Int
rightButton = 2

withoutMods :: BaseEvent -> Int -> Bool
withoutMods evt b = mouseButton b evt
    && not (mouseAltKey   evt)
    && not (mouseCtrlKey  evt)
    && not (mouseShiftKey evt)
    && not (mouseMetaKey  evt)

withCtrl :: BaseEvent -> Int -> Bool
withCtrl evt b = mouseButton b evt
    && (mouseCtrlKey evt || mouseMetaKey evt)

withAlt :: BaseEvent -> Int -> Bool
withAlt evt b = mouseButton b evt
    &&      mouseAltKey   evt
    && not (mouseCtrlKey  evt)
    && not (mouseShiftKey evt)
    && not (mouseMetaKey  evt)

withShift :: BaseEvent -> Int -> Bool
withShift evt b = mouseButton b evt
    && not (mouseAltKey   evt)
    && not (mouseCtrlKey  evt)
    &&      mouseShiftKey evt
    && not (mouseMetaKey  evt)

withCtrlAlt :: BaseEvent -> Int -> Bool
withCtrlAlt evt b = mouseButton b evt
    &&      mouseAltKey   evt
    && not (mouseShiftKey evt)
    && (mouseCtrlKey evt || not (mouseMetaKey  evt))

withCtrlShift :: BaseEvent -> Int -> Bool
withCtrlShift evt b = mouseButton b evt
    && not (mouseAltKey   evt)
    &&      mouseShiftKey evt
    && (mouseCtrlKey evt || not (mouseMetaKey  evt))

withAltShift :: BaseEvent -> Int -> Bool
withAltShift evt b = mouseButton b evt
    &&      mouseAltKey   evt
    && not (mouseCtrlKey  evt)
    &&      mouseShiftKey evt
    && not (mouseMetaKey  evt)

withCtrlAltShift :: BaseEvent -> Int -> Bool
withCtrlAltShift evt b = mouseButton b evt
    && mouseAltKey   evt
    && mouseShiftKey evt
    && (mouseCtrlKey evt || not (mouseMetaKey evt))

mousePosition :: BaseEvent -> ScreenPosition
mousePosition = \case
    Mouse evt -> fromDoubles (evt ^. pageX) (evt ^. pageY)
    _         -> def

instance Convertible Target InPortRef where
    convert (Target [nodeLoc, portId]) = InPortRef (read nodeLoc) (read portId)
    convert (Target [inPortRef])       = read inPortRef
    convert _ = error "Cannot parse Target to InPortRef"

instance Convertible Target OutPortRef where
    convert (Target [nodeLoc, portId]) = OutPortRef (read nodeLoc) (read portId)
    convert _ = error "Cannot parse Target to OutPortRef"

instance Convertible Target NodeLoc where
    convert (Target [nodeLoc]) = read nodeLoc
    convert _ = error "Cannot parse Target to NodeLoc"

instance Convertible Target VisualizerId where
    convert (Target [visId]) = read visId
    convert _ = error "Cannot parse Target to VisualizerId"

instance Convertible Target VisualizationId where
    convert (Target [visId]) = read visId
    convert _ = error "Cannot parse Target to VisualizationId"
