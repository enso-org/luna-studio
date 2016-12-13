module Reactive.Plugins.Core.Action.Backend.Control
    ( toAction
    ) where

import           Luna.Studio.Prelude

import qualified Event.Batch                      as Batch
import           Event.Event                      (Event (Batch))

import           Reactive.Commands.Command        (Command)
import           Reactive.State.Global            (State)

toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.EmpireStarted _)) = Just $ do
    error "Server crashed." -- could have done that more politely, but… let it crash
toAction _ = Nothing

