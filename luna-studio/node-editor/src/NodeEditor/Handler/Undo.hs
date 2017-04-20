module NodeEditor.Handler.Undo where

-- TODO[LJK, SB]: This should be handled via folder Basic instead of Action.Batch
import           NodeEditor.Action.Batch   (redo, undo)
import           NodeEditor.Action.Command (Command)
import           NodeEditor.Event.Event    (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut as Shortcut
import           Common.Prelude
import           NodeEditor.State.Global   (State)


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Undo _)) = Just undo
handle (Shortcut (Shortcut.Event Shortcut.Redo _)) = Just redo
handle _ = Nothing
