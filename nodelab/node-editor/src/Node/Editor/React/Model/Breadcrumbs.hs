module Node.Editor.React.Model.Breadcrumbs (
    module Node.Editor.React.Model.Breadcrumbs,
    module X,
) where

import           Empire.API.Data.Breadcrumb as X
import           Node.Editor.React.Event.Breadcrumbs    as X



type Breadcrumbs = Breadcrumb (Named BreadcrumbItem)
