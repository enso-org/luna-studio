module Luna.Studio.Handler.CodeEditor where

import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.State.App       (toggleCodeEditor)
import           Luna.Studio.Event.Event            (Event (UI))
import           Luna.Studio.Event.UI               (UIEvent (CodeEditorEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.CodeEditor as CodeEditor
import           Luna.Studio.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (CodeEditorEvent CodeEditor.ToggleCodeEditor)) = Just toggleCodeEditor
handle _                                                  = Nothing
