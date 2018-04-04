module LunaStudio.Data.GraphLocation where

import qualified Control.Lens.Aeson         as Lens
import           Data.Aeson.Types           (FromJSON (..), ToJSON (..))
import           Data.Binary                (Binary)
import           LunaStudio.Data.Breadcrumb (Breadcrumb, BreadcrumbItem)
import           Prologue


data GraphLocation = GraphLocation
    { _filePath   :: FilePath
    , _breadcrumb :: Breadcrumb BreadcrumbItem
    } deriving (Eq, Generic, Show)

makeLenses ''GraphLocation

instance Binary   GraphLocation
instance NFData   GraphLocation
instance FromJSON GraphLocation where parseJSON = Lens.parse
instance ToJSON   GraphLocation where
    toJSON     = Lens.toJSON
    toEncoding = Lens.toEncoding
