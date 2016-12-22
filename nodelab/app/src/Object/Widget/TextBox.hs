module Object.Widget.TextBox where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude
import           Object.Widget

data TextAlignment = Left | Center | Right deriving (Eq, Show, Generic)
instance ToJSON TextAlignment

data TextBox = TextBox { _position  :: Position
                       , _size      :: Size
                       , _value     :: Text
                       , _alignment :: TextAlignment
                       , _isEditing :: Bool
                       } deriving (Eq, Show, Typeable, Generic)

makeLenses ''TextBox
instance ToJSON TextBox

create :: Size -> Text -> TextAlignment -> TextBox
create s v a = TextBox def s v a False
