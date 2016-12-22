module Object.Widget.Icon where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude

import           Object.Widget


data Icon = Icon { _position :: Position
                 , _size     :: Size
                 , _shader   :: Text
                 } deriving (Eq, Show, Typeable, Generic)


makeLenses ''Icon
instance ToJSON Icon

create :: Text -> Icon
create = Icon def def
