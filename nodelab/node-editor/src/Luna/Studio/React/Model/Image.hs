module Luna.Studio.React.Model.Image where

import           Data.Aeson          (ToJSON)
import           Data.Position       (Position)
import           Data.Size           (Size)
import           Luna.Prelude



data Image = Image { _position   :: Position
                   , _size       :: Size
                   , _image      :: Text
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Image
instance ToJSON Image

create :: Size -> Text -> Image
create = Image def
