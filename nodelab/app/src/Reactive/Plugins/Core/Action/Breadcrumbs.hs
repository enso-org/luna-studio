module Reactive.Plugins.Core.Action.Breadcrumbs where

import           Event.Event                      (Event (UI))
import           Event.UI                         (UIEvent (BreadcrumbsEvent))
import qualified Luna.Studio.React.Event.Breadcrumbs          as Breadcrumbs
import           Luna.Studio.Commands.Command        (Command)
import qualified Luna.Studio.Commands.ProjectManager as ProjectManager
import           Luna.Studio.State.Global            (State)
import           Luna.Studio.Prelude



toAction :: Event -> Maybe (Command State ())
toAction (UI (BreadcrumbsEvent (Breadcrumbs.Enter bc))) = Just $ ProjectManager.enterBreadcrumbs bc
toAction _   = Nothing
