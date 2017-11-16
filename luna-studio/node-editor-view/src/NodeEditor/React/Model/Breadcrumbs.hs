module NodeEditor.React.Model.Breadcrumbs (
    module NodeEditor.React.Model.Breadcrumbs,
    module X,
) where

import           Common.Prelude
import           LunaStudio.Data.Breadcrumb         as X
import           NodeEditor.React.Event.Breadcrumbs as X



type Breadcrumbs = Breadcrumb (Named BreadcrumbItem)

isTopLevel :: Breadcrumbs -> Bool
isTopLevel = null . view items
