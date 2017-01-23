module Luna.Studio.Handler.Atom where

import           Event.Atom                  (AtomEvent (Event))
import           Event.Event                 (Event (Atom))
import           Luna.Studio.Action.Command  (Command)
import           Luna.Studio.Action.Graph    (selectAll)
import qualified Luna.Studio.Action.Searcher as Searcher
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global    (State)



toAction :: Event -> Maybe (Command State ())
toAction (Atom Event) = Just $ selectAll >> Searcher.open
-- toAction (Atom Test) = Just $ selectAll >> Searcher.open
-- toAction (Atom Test) = Just $ error "Searcher.open >> selectAll"
toAction _           = Nothing
