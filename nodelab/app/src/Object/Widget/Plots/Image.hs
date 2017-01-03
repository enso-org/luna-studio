module Object.Widget.Plots.Image where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude



data Image = Image { _position   :: Position
                   , _size       :: Size
                   , _image      :: Text
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Image
instance ToJSON Image

create :: Size -> Text -> Image
create = Image def
