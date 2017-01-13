module Luna.Studio.Action.Breadcrumbs
    ( set
    ) where

import           Empire.API.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem, Named)
import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Prelude        hiding (group, set)
import           Luna.Studio.State.Global   (State)
import qualified Luna.Studio.State.Global   as Global
import qualified Luna.Studio.React.Model.App          as App



set :: Breadcrumb (Named BreadcrumbItem)-> Command State ()
set breadcrumbs = do
    Global.withApp $ App.breadcrumbs .= breadcrumbs
