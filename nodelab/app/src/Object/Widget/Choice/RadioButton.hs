module Object.Widget.Choice.RadioButton where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2 (Vector2))
import           Luna.Studio.Prelude
import           Object.Widget

data RadioButton = RadioButton { _position :: Position
                               , _size     :: Vector2 Double
                               , _label    :: Text
                               , _selected :: Bool
                               } deriving (Eq, Show, Typeable, Generic)

makeLenses ''RadioButton

instance ToJSON RadioButton
instance IsDisplayObject RadioButton where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
