module Node.Editor.Handler.Undo where

-- TODO[LJK, SB]: This should be handled via folder Basic instead of Action.Batch
import           Node.Editor.Action.Batch   (redo, undo)
import           Node.Editor.Action.Command (Command)
import           Node.Editor.Event.Event    (Event (Shortcut))
import qualified Node.Editor.Event.Shortcut as Shortcut
import           Luna.Prelude
import           Node.Editor.State.Global   (State)


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Undo _)) = Just undo
handle (Shortcut (Shortcut.Event Shortcut.Redo _)) = Just redo
handle _ = Nothing
