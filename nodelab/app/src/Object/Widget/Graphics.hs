module Object.Widget.Graphics where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size, Vector2)
import           Luna.Studio.Prelude     hiding (Item)

import           Object.Widget.Label     (TextAlignment)



data Box = Box { _boxPosition :: Position
               } deriving (Eq, Show, Typeable, Generic)

data Item = Item { _shader :: Text
                 , _boxes  :: [Box]
                 , _boxSize   :: Size
                 , _boxOffset :: Vector2 Double
                 } deriving (Eq, Show, Typeable, Generic)

data Label = Label { _labelPosition :: Position
                   , _fontSize      :: Double
                   , _textAlignment :: TextAlignment
                   , _text          :: Text
                   } deriving (Eq, Show, Typeable, Generic)

data Graphics = Graphics { _position :: Position
                         , _size     :: Size
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
