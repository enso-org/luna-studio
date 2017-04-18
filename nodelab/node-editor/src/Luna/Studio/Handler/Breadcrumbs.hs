module Luna.Studio.Handler.Breadcrumbs where

import           Luna.Studio.Action.Basic            (enterBreadcrumbs, exitBreadcrumb)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Event.Event             (Event (Shortcut, UI))
import qualified Luna.Studio.Event.Shortcut          as Shortcut
import           Luna.Studio.Event.UI                (UIEvent (BreadcrumbsEvent))
import           Luna.Prelude
import qualified Luna.Studio.React.Event.Breadcrumbs as Breadcrumbs
import           Luna.Studio.State.Global            (State)



handle :: Event -> Maybe (Command State ())
handle (UI (BreadcrumbsEvent (Breadcrumbs.Enter bc))) = Just $ enterBreadcrumbs bc
handle (Shortcut (Shortcut.Event command _))          = Just $ handleCommand command
handle _   = Nothing



handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.ExitGraph -> exitBreadcrumb
    _                  -> return ()
