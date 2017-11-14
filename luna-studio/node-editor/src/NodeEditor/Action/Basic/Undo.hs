module NodeEditor.Action.Basic.Undo where

import           Common.Action.Command          (Command)
import           Common.Prelude
import           NodeEditor.Action.Batch        as Batch
import           NodeEditor.Action.State.Action (checkIfActionPerfoming)
import           NodeEditor.State.Action        (searcherAction, textPortControlEditAction)
import           NodeEditor.State.Global        (State)


undo :: Command State ()
undo = do
    searcherActive    <- checkIfActionPerfoming searcherAction
    editControlActive <- checkIfActionPerfoming textPortControlEditAction
    unless (searcherActive || editControlActive) $ Batch.undo


redo :: Command State ()
redo = do
    searcherActive    <- checkIfActionPerfoming searcherAction
    editControlActive <- checkIfActionPerfoming textPortControlEditAction
    unless (searcherActive || editControlActive) $ Batch.redo

