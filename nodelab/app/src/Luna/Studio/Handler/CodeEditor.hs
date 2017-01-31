module Luna.Studio.Handler.CodeEditor where

import qualified Luna.Studio.Action.CodeEditor      as CodeEditor
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Event.Event            (Event (UI))
import           Luna.Studio.Event.UI               (UIEvent (CodeEditorEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.CodeEditor as CodeEditor
import           Luna.Studio.State.Global           (State)



handle :: Event -> Maybe (Command State ())
handle (UI (CodeEditorEvent CodeEditor.ToggleCodeEditor)) = Just $ CodeEditor.toggle
handle _   = Nothing
