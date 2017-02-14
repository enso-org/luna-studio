module Luna.Studio.Handler.Undo where

import           Luna.Studio.Action.Batch   (requestRedo, requestUndo)
import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Event.Event    (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut as Shortcut
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global   (State)


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Undo _)) = Just requestUndo
handle (Shortcut (Shortcut.Event Shortcut.Redo _)) = Just requestRedo
handle _ = Nothing
