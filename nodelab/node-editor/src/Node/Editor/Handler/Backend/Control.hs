module Node.Editor.Handler.Backend.Control
    ( handle
    ) where

import           Luna.Prelude
import           Luna.Report

import qualified Node.Editor.Event.Batch    as Batch
import           Node.Editor.Event.Event    (Event (Batch))

import           Node.Editor.Action.Command (Command)
import           Node.Editor.State.Global   (State)



handle :: Event -> Maybe (Command State ())
handle (Batch (Batch.EmpireStarted _)) = Just $ do
    fatal "Server crashed."
    -- Batch.getProgram

handle _ = Nothing
