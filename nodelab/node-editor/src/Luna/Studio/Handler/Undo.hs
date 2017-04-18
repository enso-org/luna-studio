module Luna.Studio.Handler.Undo where

-- TODO[LJK, SB]: This should be handled via folder Basic instead of Action.Batch
import           Luna.Studio.Action.Batch   (redo, undo)
import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Event.Event    (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut as Shortcut
import           Luna.Prelude
import           Luna.Studio.State.Global   (State)


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Undo _)) = Just undo
handle (Shortcut (Shortcut.Event Shortcut.Redo _)) = Just redo
handle _ = Nothing
