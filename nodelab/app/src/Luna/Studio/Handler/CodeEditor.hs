module Luna.Studio.Handler.CodeEditor where

import           Event.Event                  (Event (UI))
import           Event.UI                     (UIEvent (CodeEditorEvent))
import qualified Luna.Studio.React.Event.CodeEditor       as CodeEditor
import qualified Luna.Studio.Action.CodeEditor as CodeEditor
import           Luna.Studio.Action.Command    (Command)
import           Luna.Studio.State.Global        (State)
import           Luna.Studio.Prelude



toAction :: Event -> Maybe (Command State ())
toAction (UI (CodeEditorEvent CodeEditor.ToggleCodeEditor)) = Just $ CodeEditor.toggle
toAction _   = Nothing
