module Object.Widget.Choice where

import           Data.Aeson          (ToJSON)
import           Luna.Studio.Prelude   hiding (Choice)
import           Luna.Studio.Data.Vector

import           Object.Widget
import           Object.Widget.Group (Group (..))

data Choice = Choice { _position :: Vector2 Double
                     , _size     :: Vector2 Double
                     , _label    :: Text
                     , _options  :: [Text]
                     , _value    :: Word
                     } deriving (Eq, Show, Typeable, Generic)

makeLenses ''Choice

create :: Size -> Text -> [Text] -> Word -> Choice
create s l o v = Choice def s l o v

instance ToJSON          Choice
instance IsDisplayObject Choice where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True

toGroup :: Choice -> Group
toGroup c = Group (c ^. position) (c ^. size) True def
