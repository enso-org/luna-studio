module Empire.API.Data.GraphLocation where

import           Data.Binary                (Binary)
import           Prologue

import           Empire.API.Data.Breadcrumb (Breadcrumb, BreadcrumbItem)

data GraphLocation = GraphLocation { _filePath   :: FilePath
                                   , _breadcrumb :: Breadcrumb BreadcrumbItem
                                   } deriving (Generic, Eq, NFData, Show)

makeLenses ''GraphLocation

instance Binary GraphLocation
