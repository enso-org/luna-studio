module Node.Editor.Action.Basic.SetNodeExpression where

import qualified Node.Editor.Action.Batch                    as Batch
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.State.NodeEditor         (inGraph, modifyExpressionNode)
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (NodeLoc, expression)
import           Node.Editor.State.Global                    (State)


setNodeExpression :: NodeLoc -> Text -> Command State ()
setNodeExpression nl update =
    whenM (localSetNodeExpression nl update) $ Batch.setNodeExpression nl update

localSetNodeExpression :: NodeLoc -> Text -> Command State Bool
localSetNodeExpression nl update = do
    modifyExpressionNode nl $ expression .= update
    inGraph nl
