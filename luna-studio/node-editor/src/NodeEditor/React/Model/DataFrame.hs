{-# LANGUAGE StrictData #-}
module NodeEditor.React.Model.DataFrame where

import           Common.Prelude

data DataFrame = DataFrame { _headers   :: [Text]
                           , _rows      :: [[Text]]
                           } deriving (Eq, Show, Typeable, Generic)


makeLenses ''DataFrame

create :: [Text] -> [[Text]] -> DataFrame
create = DataFrame
