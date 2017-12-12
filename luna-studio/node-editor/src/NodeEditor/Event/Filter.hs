module NodeEditor.Event.Filter where

import           Common.Data.Event       (eventName)
import           Common.Prelude
import           NodeEditor.Event.Event  (Event)
import           NodeEditor.State.Global (State)
import           JS.Atom                 (acceptEvent)


filterEvents :: State -> Event -> IO State -> IO State
filterEvents state event action = if acceptEvent event
    then action
    else return state
