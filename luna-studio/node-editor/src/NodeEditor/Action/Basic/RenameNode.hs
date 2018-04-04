module NodeEditor.Action.Basic.RenameNode where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified NodeEditor.Action.Batch                    as Batch
import qualified NodeEditor.Action.State.NodeEditor         as NodeEditor
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.State.Global                    (State)


renameNode :: NodeLoc -> Text -> Command State ()
renameNode nl update =
    whenM (localRenameNode nl $ Just update) $ Batch.renameNode nl update

localRenameNode :: NodeLoc -> Maybe Text -> Command State Bool
localRenameNode nl update = do
    NodeEditor.modifyExpressionNode nl $ Node.name .= update
    NodeEditor.inGraph nl
