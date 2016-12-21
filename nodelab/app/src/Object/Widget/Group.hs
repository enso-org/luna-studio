module Object.Widget.Group where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, Vector2)
import           Luna.Studio.Prelude
import qualified Style.Group             as Style
import           Style.Types

import           Object.Widget


data Group = Group { _position   :: Position
                   , _size       :: Vector2 Double
                   , _visible    :: Bool
                   , _style      :: Style
                   } deriving (Eq, Show, Typeable, Generic)

data Style = Style { _background   :: Maybe Color
                   , _padding      :: Padding
                   , _borderRadius :: (Double, Double, Double, Double)
                   } deriving (Eq, Show, Generic)

instance Default Style where
    def = Style Nothing def Style.borderRadius

makeLenses ''Group
makeLenses ''Style
instance ToJSON Group
instance ToJSON Style
instance ToJSON Padding

create :: Group
create = Group def def True def


instance IsDisplayObject Group where
    widgetPosition = position
    widgetSize     = size
    widgetVisible  = visible
