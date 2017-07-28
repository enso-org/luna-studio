module NodeEditor.Handler.Backend.Control
    ( handle
    ) where

import           Common.Prelude
import           Common.Report

import qualified NodeEditor.Event.Batch  as Batch
import           NodeEditor.Event.Event  (Event (Batch))

import           Common.Action.Command   (Command)
import           NodeEditor.State.Global (State)



handle :: Event -> Maybe (Command State ())
handle (Batch (Batch.EmpireStarted _)) = Just $ do
    fatal "Server crashed."
    -- Batch.getProgram

handle _ = Nothing
