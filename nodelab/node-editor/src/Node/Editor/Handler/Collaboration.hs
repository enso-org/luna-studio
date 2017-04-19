module Node.Editor.Handler.Collaboration where

import           Node.Editor.Action.Basic   (updateCollaboration)
import           Node.Editor.Action.Command (Command)
import           Node.Editor.Event.Event    (Event (Tick))
import           Luna.Prelude
import           Node.Editor.State.Global   (State)


handle :: Event -> Maybe (Command State ())
handle Tick = Just $ updateCollaboration
handle _    = Nothing
