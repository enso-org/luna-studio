module Object.Widget.Choice where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude     hiding (Choice)

import           Object.Widget.Group     (Group (..))

data Choice = Choice { _position :: Position
                     , _size     :: Size
                     , _label    :: Text
                     , _options  :: [Text]
                     , _value    :: Word
                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Choice

create :: Size -> Text -> [Text] -> Word -> Choice
create s l o v = Choice def s l o v

instance ToJSON          Choice

toGroup :: Choice -> Group
toGroup c = Group (c ^. position) (c ^. size) True def
