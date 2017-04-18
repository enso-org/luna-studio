module Luna.Studio.React.Model.Breadcrumbs (
    module Luna.Studio.React.Model.Breadcrumbs,
    module X,
) where

import           Empire.API.Data.Breadcrumb as X
import           Luna.Studio.React.Event.Breadcrumbs    as X



type Breadcrumbs = Breadcrumb (Named BreadcrumbItem)
