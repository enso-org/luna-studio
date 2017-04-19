module Node.Editor.Action.Basic.RenameNode where

import qualified Node.Editor.Action.Batch                    as Batch
import           Node.Editor.Action.Command                  (Command)
import qualified Node.Editor.Action.State.NodeEditor         as NodeEditor
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (NodeLoc)
import qualified Node.Editor.React.Model.Node.ExpressionNode as Node
import           Node.Editor.State.Global                    (State)


renameNode :: NodeLoc -> Text -> Command State ()
renameNode nl update =
    whenM (localRenameNode nl update) $ Batch.renameNode nl update

localRenameNode :: NodeLoc -> Text -> Command State Bool
localRenameNode nl update = do
    NodeEditor.modifyExpressionNode nl $ do
        Node.name         ?= update
        Node.isNameEdited .= False
    NodeEditor.inGraph nl
