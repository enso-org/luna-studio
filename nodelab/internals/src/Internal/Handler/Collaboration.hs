module Internal.Handler.Collaboration where

import           Internal.Action.Basic   (updateCollaboration)
import           Internal.Action.Command (Command)
import           Internal.Event.Event    (Event (Tick))
import           Internal.Prelude
import           Internal.State.Global   (State)


handle :: Event -> Maybe (Command State ())
handle Tick = Just $ updateCollaboration
handle _    = Nothing
