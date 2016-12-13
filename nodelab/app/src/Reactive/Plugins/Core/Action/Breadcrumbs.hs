module Reactive.Plugins.Core.Action.Breadcrumbs where

import           Event.Event                      (Event (UI))
import           Event.UI                         (UIEvent (BreadcrumbsEvent))
import qualified React.Event.Breadcrumbs          as Breadcrumbs
import           Reactive.Commands.Command        (Command)
import qualified Reactive.Commands.ProjectManager as ProjectManager
import           Reactive.State.Global            (State)
import           Luna.Studio.Prelude



toAction :: Event -> Maybe (Command State ())
toAction (UI (BreadcrumbsEvent (Breadcrumbs.Enter bc))) = Just $ ProjectManager.enterBreadcrumbs bc
toAction _   = Nothing
