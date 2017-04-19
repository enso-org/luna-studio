module Node.Editor.Handler.Breadcrumbs where

import           Node.Editor.Action.Basic            (enterBreadcrumbs, exitBreadcrumb)
import           Node.Editor.Action.Command          (Command)
import           Node.Editor.Event.Event             (Event (Shortcut, UI))
import qualified Node.Editor.Event.Shortcut          as Shortcut
import           Node.Editor.Event.UI                (UIEvent (BreadcrumbsEvent))
import           Luna.Prelude
import qualified Node.Editor.React.Event.Breadcrumbs as Breadcrumbs
import           Node.Editor.State.Global            (State)



handle :: Event -> Maybe (Command State ())
handle (UI (BreadcrumbsEvent (Breadcrumbs.Enter bc))) = Just $ enterBreadcrumbs bc
handle (Shortcut (Shortcut.Event command _))          = Just $ handleCommand command
handle _   = Nothing



handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.ExitGraph -> exitBreadcrumb
    _                  -> return ()
