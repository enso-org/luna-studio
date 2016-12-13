module Object.Widget.Icon where

import           Data.Aeson        (ToJSON)
import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import           Object.Widget


data Icon = Icon { _position :: Vector2 Double
                 , _size     :: Vector2 Double
                 , _shader   :: Text
                 } deriving (Eq, Show, Typeable, Generic)


makeLenses ''Icon
instance ToJSON Icon

create :: Text -> Icon
create = Icon def def

instance IsDisplayObject Icon where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = to $ const True
