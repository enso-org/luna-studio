module Object.Widget.DataFrame where

import           Utils.PreludePlus
import           Utils.Vector
import           Object.Widget
import           Data.Aeson (ToJSON)

data DataFrame = DataFrame { _position  :: Vector2 Double
                           , _size      :: Vector2 Double
                           , _headers   :: [Text]
                           , _rows      :: [[Text]]
                           } deriving (Eq, Show, Typeable, Generic)

makeLenses ''DataFrame
instance ToJSON DataFrame

create :: Size -> [Text] -> [[Text]] -> DataFrame
create s h v = DataFrame def s h v
