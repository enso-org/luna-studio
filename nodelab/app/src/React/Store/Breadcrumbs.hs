module React.Store.Breadcrumbs (
    module React.Store.Breadcrumbs,
    module X,
) where

import           Empire.API.Data.Breadcrumb as X
import           React.Event.Breadcrumbs    as X



type Breadcrumbs = Breadcrumb (Named BreadcrumbItem)
