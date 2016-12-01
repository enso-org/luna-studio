module Reactive.Commands.Breadcrumbs (
    set
) where

import           Utils.PreludePlus          hiding (group, set)

import           Empire.API.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem, Named)
import qualified React.Store                as Store
import           Reactive.Commands.Command  (Command)
import           Reactive.State.Global      (State)
import qualified Reactive.State.Global      as Global



set :: Breadcrumb (Named BreadcrumbItem)-> Command State ()
set breadcrumbs = do
    Global.withBreadcrumbs $ Store.modify_ $ const breadcrumbs
