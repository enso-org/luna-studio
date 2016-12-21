module Object.Widget.Graphics where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2)
import           Luna.Studio.Prelude     hiding (Item)

import           Object.Widget
import           Object.Widget.Label     (TextAlignment)

data Box = Box { _boxPosition :: Position
               } deriving (Eq, Show, Typeable, Generic)

data Item = Item { _shader :: Text
                 , _boxes  :: [Box]
                 , _boxSize   :: Vector2 Double
                 , _boxOffset :: Vector2 Double
                 } deriving (Eq, Show, Typeable, Generic)

data Label = Label { _labelPosition :: Position
                   , _fontSize      :: Double
                   , _textAlignment :: TextAlignment
                   , _text          :: Text
                   } deriving (Eq, Show, Typeable, Generic)

data Graphics = Graphics { _position :: Position
                         , _size     :: Vector2 Double
                         , _items    :: [Item]
                         , _labels   :: [Label]
                         } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Box
makeLenses ''Item
makeLenses ''Label
makeLenses ''Graphics

instance ToJSON Box
instance ToJSON Item
instance ToJSON Label
instance ToJSON Graphics

create :: Size -> [Item] -> [Label] -> Graphics
create = Graphics def

instance IsDisplayObject Graphics where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
