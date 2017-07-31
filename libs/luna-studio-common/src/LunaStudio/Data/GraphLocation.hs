module LunaStudio.Data.GraphLocation where

import           Control.Lens.Aeson         (lensJSONParse, lensJSONToEncoding, lensJSONToJSON)
import           Data.Aeson.Types           (FromJSON (..), ToJSON (..))
import           Data.Binary                (Binary)
import           LunaStudio.Data.Breadcrumb (Breadcrumb, BreadcrumbItem)
import           Prologue


data GraphLocation = GraphLocation { _filePath   :: FilePath
                                   , _breadcrumb :: Breadcrumb BreadcrumbItem
                                   } deriving (Eq, Generic, NFData, Show)

makeLenses ''GraphLocation
instance Binary   GraphLocation
instance FromJSON GraphLocation where parseJSON = lensJSONParse
instance ToJSON   GraphLocation where
    toJSON     = lensJSONToJSON
    toEncoding = lensJSONToEncoding
