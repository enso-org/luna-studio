module LunaStudio.Data.GraphLocation where

import           Data.Binary                (Binary)
import           Prologue
import           LunaStudio.Data.Breadcrumb (Breadcrumb, BreadcrumbItem)


data GraphLocation = GraphLocation { _filePath   :: FilePath
                                   , _breadcrumb :: Breadcrumb BreadcrumbItem
                                   } deriving (Eq, Generic, NFData, Show)

makeLenses ''GraphLocation
instance Binary GraphLocation
