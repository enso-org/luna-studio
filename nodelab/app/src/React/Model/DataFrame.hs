module React.Model.DataFrame where

import           Utils.PreludePlus
import           Data.Aeson (ToJSON)

data DataFrame = DataFrame { _headers   :: [Text]
                           , _rows      :: [[Text]]
                           } deriving (Eq, Show, Typeable, Generic)


makeLenses ''DataFrame
instance ToJSON DataFrame

create :: [Text] -> [[Text]] -> DataFrame
create h v = DataFrame h v
