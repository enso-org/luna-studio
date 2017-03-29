module Luna.Studio.Action.Basic.SetNodeExpression where

import qualified Luna.Studio.Action.Batch                    as Batch
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (inGraph, modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeLoc, expression)
import           Luna.Studio.State.Global                    (State)


setNodeExpression :: NodeLoc -> Text -> Command State ()
setNodeExpression nl update =
    whenM (localSetNodeExpression nl update) $ Batch.setNodeExpression nl update

localSetNodeExpression :: NodeLoc -> Text -> Command State Bool
localSetNodeExpression nl update = do
    modifyExpressionNode nl $ expression .= update
    inGraph nl
