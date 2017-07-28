module NodeEditor.Handler.Breadcrumbs where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           NodeEditor.Action.Basic            (enterBreadcrumbs, exitBreadcrumb)
import           NodeEditor.Event.Event             (Event (Shortcut, UI))
import qualified NodeEditor.Event.Shortcut          as Shortcut
import           NodeEditor.Event.UI                (UIEvent (BreadcrumbsEvent))
import qualified NodeEditor.React.Event.Breadcrumbs as Breadcrumbs
import           NodeEditor.State.Global            (State)



handle :: Event -> Maybe (Command State ())
handle (UI (BreadcrumbsEvent (Breadcrumbs.Enter bc))) = Just $ enterBreadcrumbs bc
handle (Shortcut (Shortcut.Event command _))          = Just $ handleCommand command
handle _   = Nothing



handleCommand :: Shortcut.Command -> Command State ()
handleCommand = \case
    Shortcut.ExitGraph -> exitBreadcrumb
    _                  -> return ()
