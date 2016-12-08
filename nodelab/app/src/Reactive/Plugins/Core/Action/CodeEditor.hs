module Reactive.Plugins.Core.Action.CodeEditor where

import           Event.Event                  (Event (UI))
import           Event.UI                     (UIEvent (CodeEditorEvent))
import qualified React.Event.CodeEditor       as CodeEditor
import qualified Reactive.Commands.CodeEditor as CodeEditor
import           Reactive.Commands.Command    (Command)
import           Reactive.State.Global        (State)
import           Utils.PreludePlus



toAction :: Event -> Maybe (Command State ())
toAction (UI (CodeEditorEvent CodeEditor.ToggleCodeEditor)) = Just $ CodeEditor.toggle
toAction _   = Nothing
