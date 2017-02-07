module Luna.Studio.Handler.Undo where

import           Luna.Studio.Action.Batch           (requestRedo, requestUndo)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.State.Global             (State)
import           Luna.Studio.Prelude
import           Luna.Studio.Event.Event         (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut      as Shortcut


handle :: Event -> Maybe (Command State ())
handle (Shortcut Shortcut.Undo) = Just requestUndo
handle (Shortcut Shortcut.Redo) = Just requestRedo
handle _ = Nothing
