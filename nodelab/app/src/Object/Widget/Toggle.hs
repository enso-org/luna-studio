module Object.Widget.Toggle where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2)
import           Luna.Studio.Prelude
import           Object.Widget

data Toggle = Toggle { _position :: Position
                     , _size     :: Vector2 Double
                     , _label    :: Text
                     , _value    :: Bool
                     , _enabled  :: Bool
                     , _focused  :: Bool
                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Toggle
instance ToJSON Toggle

create :: Size -> Text -> Bool -> Toggle
create s l v = Toggle def s l v True False

instance IsDisplayObject Toggle where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
