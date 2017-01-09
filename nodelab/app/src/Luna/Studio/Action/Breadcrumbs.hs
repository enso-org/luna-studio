module Luna.Studio.Action.Breadcrumbs
    ( set
    ) where

import           Empire.API.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem, Named)
import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Prelude        hiding (group, set)
import qualified Luna.Studio.React.Store    as Store
import           Luna.Studio.State.Global   (State)
import qualified Luna.Studio.State.Global   as Global



set :: Breadcrumb (Named BreadcrumbItem)-> Command State ()
set breadcrumbs = do
    Global.withBreadcrumbs $ Store.modify_ $ const breadcrumbs
