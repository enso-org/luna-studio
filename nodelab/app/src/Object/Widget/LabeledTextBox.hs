module Object.Widget.LabeledTextBox where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector
import           Object.Widget
import           Data.Aeson (ToJSON)

data LabeledTextBox = LabeledTextBox { _position  :: Vector2 Double
                                     , _size      :: Vector2 Double
                                     , _label     :: Text
                                     , _value     :: Text
                                     , _isEditing :: Bool
                                     } deriving (Eq, Show, Typeable, Generic)

makeLenses    ''LabeledTextBox
instance ToJSON LabeledTextBox

create :: Size -> Text -> Text -> LabeledTextBox
create s l v = LabeledTextBox def s l v False

instance IsDisplayObject LabeledTextBox where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True