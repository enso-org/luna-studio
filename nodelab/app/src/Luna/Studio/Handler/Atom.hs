module Luna.Studio.Handler.Atom where

import           Luna.Studio.Action.Command  (Command)
import           Luna.Studio.Action.Graph    (selectAll)
import qualified Luna.Studio.Action.Searcher as Searcher
import           Luna.Studio.Event.Atom      (AtomEvent (..))
import           Luna.Studio.Event.Event     (Event (Atom))
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global    (State)



toAction :: Event -> Maybe (Command State ())
toAction (Atom OpenSearcher) = Just Searcher.open
toAction (Atom Cancel)       = Just Searcher.close
toAction (Atom Accept)       = Just Searcher.accept
toAction (Atom SelectAll)    = Just selectAll
toAction _                   = Nothing
