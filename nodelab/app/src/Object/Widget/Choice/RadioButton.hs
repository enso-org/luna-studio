module Object.Widget.Choice.RadioButton where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude
import           Object.Widget

data RadioButton = RadioButton { _position :: Position
                               , _size     :: Size
                               , _label    :: Text
                               , _selected :: Bool
                               } deriving (Eq, Show, Typeable, Generic)

makeLenses ''RadioButton

instance ToJSON RadioButton
