module NodeEditor.React.Model.Breadcrumbs (
    module NodeEditor.React.Model.Breadcrumbs,
    module X,
) where

import           Empire.API.Data.Breadcrumb as X
import           NodeEditor.React.Event.Breadcrumbs    as X



type Breadcrumbs = Breadcrumb (Named BreadcrumbItem)
