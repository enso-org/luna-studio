module Object.Widget.Label where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2)
import           Luna.Studio.Prelude     hiding (Either (..))
import           Object.Widget

data TextAlignment = Left | Center | Right deriving (Eq, Show, Generic)
data FontStyle     = SansSerif | Monospace deriving (Eq, Show, Generic)
instance ToJSON TextAlignment
instance ToJSON FontStyle

instance Default FontStyle where def = SansSerif

data Label = Label { _position  :: Position
                   , _size      :: Vector2 Double
                   , _alignment :: TextAlignment
                   , _fontStyle :: FontStyle
                   , _label     :: Text
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses    ''Label
instance ToJSON Label

create :: Size -> Text -> Label
create size' = Label def size' Left SansSerif

instance IsDisplayObject Label where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
