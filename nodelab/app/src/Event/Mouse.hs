module Event.Mouse where


import Luna.Studio.Prelude

import Event.Keyboard  (KeyMods(..))
import Luna.Studio.Data.Vector
import Object.UITypes
import Data.Aeson (ToJSON)

data MouseButton = NoButton
                 | LeftButton
                 | MiddleButton
                 | RightButton
                 deriving (Eq, Show, Generic)



toMouseButton :: Int -> MouseButton
toMouseButton 1  = LeftButton
toMouseButton 2  = MiddleButton
toMouseButton 3  = RightButton
toMouseButton _  = NoButton

type MousePosition = Vector2 Int

data Type = Pressed
          | Released
          | Moved
          | Clicked
          | DblClicked
          | Wheel (Vector2 Double)
          deriving (Eq, Show, Generic)

data EventWidget = EventWidget { _widgetId    :: WidgetId
                               , _worldMatrix :: [Double]
                               , _scene       :: SceneType
                               } deriving (Eq, Show, Generic)

makeLenses ''EventWidget

data Event a = Event { _tpe         :: Type
                     , _position    :: Vector2 a
                     , _button      :: MouseButton
                     , _keyMods     :: KeyMods
                     , _widget      :: Maybe EventWidget
                     } deriving (Eq, Show, Typeable, Generic)

type RawEvent  = Event Int
type Event'    = Event Double

makeLenses ''Event

instance ToJSON (Event Int)
instance ToJSON (Event Double)
instance ToJSON Type
instance ToJSON MouseButton
instance ToJSON EventWidget
