module React.Store.Breadcrumbs (
    module React.Store.Breadcrumbs,
    module X,
) where

import           Empire.API.Data.Breadcrumb as X (Breadcrumb (..), BreadcrumbItem, Named)



type Breadcrumbs = Breadcrumb (Named BreadcrumbItem)
