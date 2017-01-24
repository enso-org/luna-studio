module Luna.Studio.Handler.Breadcrumbs where

import           Luna.Studio.Action.Command          (Command)
import qualified Luna.Studio.Action.ProjectManager   as ProjectManager
import           Luna.Studio.Event.Event             (Event (UI))
import           Luna.Studio.Event.UI                (UIEvent (BreadcrumbsEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Breadcrumbs as Breadcrumbs
import           Luna.Studio.State.Global            (State)



handle :: Event -> Maybe (Command State ())
handle (UI (BreadcrumbsEvent (Breadcrumbs.Enter bc))) = Just $ ProjectManager.enterBreadcrumbs bc
handle _   = Nothing
