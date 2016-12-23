module Object.Widget.LabeledTextBox where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude
import           Object.Widget

data LabeledTextBox = LabeledTextBox { _position  :: Position
                                     , _size      :: Size
                                     , _label     :: Text
                                     , _value     :: Text
                                     , _isEditing :: Bool
                                     } deriving (Eq, Show, Typeable, Generic)

makeLenses    ''LabeledTextBox
instance ToJSON LabeledTextBox

create :: Size -> Text -> Text -> LabeledTextBox
create s l v = LabeledTextBox def s l v False
