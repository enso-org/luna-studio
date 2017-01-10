module Luna.Studio.Handler.Backend.Control
    ( toAction
    ) where

import           Luna.Studio.Prelude

import qualified Event.Batch                      as Batch
import           Event.Event                      (Event (Batch))

import           Luna.Studio.Action.Command        (Command)
import           Luna.Studio.State.Global            (State)

toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.EmpireStarted _)) = Just $ do
    error "Server crashed." -- could have done that more politely, but… let it crash
toAction _ = Nothing

