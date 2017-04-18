module Luna.Studio.Action.Basic.RenameNode where

import qualified Luna.Studio.Action.Batch                    as Batch
import           Luna.Studio.Action.Command                  (Command)
import qualified Luna.Studio.Action.State.NodeEditor         as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeLoc)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as Node
import           Luna.Studio.State.Global                    (State)


renameNode :: NodeLoc -> Text -> Command State ()
renameNode nl update =
    whenM (localRenameNode nl update) $ Batch.renameNode nl update

localRenameNode :: NodeLoc -> Text -> Command State Bool
localRenameNode nl update = do
    NodeEditor.modifyExpressionNode nl $ do
        Node.name         ?= update
        Node.isNameEdited .= False
    NodeEditor.inGraph nl
