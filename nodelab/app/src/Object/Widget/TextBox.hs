module Object.Widget.TextBox where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2)
import           Luna.Studio.Prelude
import           Object.Widget

data TextAlignment = Left | Center | Right deriving (Eq, Show, Generic)
instance ToJSON TextAlignment

data TextBox = TextBox { _position  :: Position
                       , _size      :: Vector2 Double
                       , _value     :: Text
                       , _alignment :: TextAlignment
                       , _isEditing :: Bool
                       } deriving (Eq, Show, Typeable, Generic)

makeLenses ''TextBox
instance ToJSON TextBox

create :: Size -> Text -> TextAlignment -> TextBox
create s v a = TextBox def s v a False

instance IsDisplayObject TextBox where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
