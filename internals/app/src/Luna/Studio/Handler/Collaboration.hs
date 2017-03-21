module Luna.Studio.Handler.Collaboration where

import           Luna.Studio.Action.Basic   (updateCollaboration)
import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Event.Event    (Event (Tick))
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global   (State)


handle :: Event -> Maybe (Command State ())
handle Tick = Just $ updateCollaboration
handle _    = Nothing
