module Object.Widget.Plots.Image where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2)
import           Luna.Studio.Prelude

import           Object.Widget


data Image = Image { _position   :: Position
                   , _size       :: Vector2 Double
                   , _image      :: Text
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Image
instance ToJSON Image

create :: Size -> Text -> Image
create = Image def

instance IsDisplayObject Image where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
