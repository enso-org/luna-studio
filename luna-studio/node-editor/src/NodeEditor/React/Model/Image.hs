module NodeEditor.React.Model.Image where

import           Common.Prelude
import           Data.Aeson               (ToJSON)
import           Empire.API.Data.Position (Position)
import           Empire.API.Data.Size     (Size)



data Image = Image { _position   :: Position
                   , _size       :: Size
                   , _image      :: Text
                   } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Image
instance ToJSON Image

create :: Size -> Text -> Image
create = Image def
