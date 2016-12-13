module Reactive.Commands.Breadcrumbs (
    set
) where

import           Luna.Studio.Prelude          hiding (group, set)

import           Empire.API.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem, Named)
import qualified Luna.Studio.React.Store                as Store
import           Reactive.Commands.Command  (Command)
import           Luna.Studio.State.Global      (State)
import qualified Luna.Studio.State.Global      as Global



set :: Breadcrumb (Named BreadcrumbItem)-> Command State ()
set breadcrumbs = do
    Global.withBreadcrumbs $ Store.modify_ $ const breadcrumbs
