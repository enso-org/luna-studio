module NodeEditor.Action.Basic.SetNodeExpression where

import qualified NodeEditor.Action.Batch                    as Batch
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.NodeEditor         (inGraph, modifyExpressionNode)
import           Common.Prelude
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, expression)
import           NodeEditor.State.Global                    (State)


setNodeExpression :: NodeLoc -> Text -> Command State ()
setNodeExpression nl update =
    whenM (localSetNodeExpression nl update) $ Batch.setNodeExpression nl update

localSetNodeExpression :: NodeLoc -> Text -> Command State Bool
localSetNodeExpression nl update = do
    modifyExpressionNode nl $ expression .= update
    inGraph nl
