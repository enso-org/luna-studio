module NodeEditor.Handler.CodeEditor where

import           NodeEditor.Action.Command         (Command)
import           NodeEditor.Action.State.App       (toggleCodeEditor)
import           NodeEditor.Event.Event            (Event (UI))
import           NodeEditor.Event.UI               (UIEvent (CodeEditorEvent))
import           Common.Prelude
import qualified NodeEditor.React.Event.CodeEditor as CodeEditor
import           NodeEditor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (CodeEditorEvent CodeEditor.ToggleCodeEditor)) = Just toggleCodeEditor
handle _                                                  = Nothing
