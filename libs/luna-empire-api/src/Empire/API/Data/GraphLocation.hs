module Empire.API.Data.GraphLocation where

import           Data.Binary                (Binary)
import           Prologue

import           Empire.API.Data.Breadcrumb (Breadcrumb, BreadcrumbItem)
import           Empire.API.Data.Library    (LibraryId)
import           Empire.API.Data.Project    (ProjectId)

data GraphLocation = GraphLocation { _projectId  :: ProjectId
                                   , _libraryId  :: LibraryId
                                   , _breadcrumb :: Breadcrumb BreadcrumbItem
                                   } deriving (Generic, Eq, NFData, Show)

makeLenses ''GraphLocation

instance Binary GraphLocation
