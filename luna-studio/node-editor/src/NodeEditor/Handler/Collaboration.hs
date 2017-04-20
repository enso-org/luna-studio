module NodeEditor.Handler.Collaboration where

import           NodeEditor.Action.Basic   (updateCollaboration)
import           NodeEditor.Action.Command (Command)
import           NodeEditor.Event.Event    (Event (Tick))
import           Common.Prelude
import           NodeEditor.State.Global   (State)


handle :: Event -> Maybe (Command State ())
handle Tick = Just $ updateCollaboration
handle _    = Nothing
