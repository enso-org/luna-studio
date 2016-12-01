module Reactive.Plugins.Core.Action.App where

import           Event.Event                  (Event (UI))
import           Event.UI                     (UIEvent (AppEvent))
import qualified React.Event.App              as App
import qualified Reactive.Commands.CodeEditor as CodeEditor
import           Reactive.Commands.Command    (Command)
import           Reactive.State.Global        (State)
import           Utils.PreludePlus



toAction :: Event -> Maybe (Command State ())
toAction (UI (AppEvent App.ToggleCodeEditor)) = Just $ CodeEditor.toggle
toAction _   = Nothing
