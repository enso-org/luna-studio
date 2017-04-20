module NodeEditor.React.Model.DataFrame where

import           Common.Prelude
import           Data.Aeson (ToJSON)

data DataFrame = DataFrame { _headers   :: [Text]
                           , _rows      :: [[Text]]
                           } deriving (Eq, Show, Typeable, Generic)


makeLenses ''DataFrame
instance ToJSON DataFrame

create :: [Text] -> [[Text]] -> DataFrame
create = DataFrame
