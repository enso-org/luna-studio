module Node.Editor.Handler.CodeEditor where

import           Node.Editor.Action.Command         (Command)
import           Node.Editor.Action.State.App       (toggleCodeEditor)
import           Node.Editor.Event.Event            (Event (UI))
import           Node.Editor.Event.UI               (UIEvent (CodeEditorEvent))
import           Luna.Prelude
import qualified Node.Editor.React.Event.CodeEditor as CodeEditor
import           Node.Editor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (CodeEditorEvent CodeEditor.ToggleCodeEditor)) = Just toggleCodeEditor
handle _                                                  = Nothing
