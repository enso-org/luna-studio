module NodeEditor.Action.Basic.RenameNode where

import qualified NodeEditor.Action.Batch                    as Batch
import           NodeEditor.Action.Command                  (Command)
import qualified NodeEditor.Action.State.NodeEditor         as NodeEditor
import           Common.Prelude
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.State.Global                    (State)


renameNode :: NodeLoc -> Text -> Command State ()
renameNode nl update =
    whenM (localRenameNode nl update) $ Batch.renameNode nl update

localRenameNode :: NodeLoc -> Text -> Command State Bool
localRenameNode nl update = do
    NodeEditor.modifyExpressionNode nl $ do
        Node.name         ?= update
        Node.isNameEdited .= False
    NodeEditor.inGraph nl
