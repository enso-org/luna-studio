module Luna.Studio.Action.Basic.SetNodeExpression where

import qualified Luna.Studio.Action.Batch                    as Batch
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (inGraph, modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeId, expression)
import           Luna.Studio.State.Global                    (State)


setNodeExpression :: NodeId -> Text -> Command State ()
setNodeExpression nid update =
    whenM (localSetNodeExpression nid update) $ Batch.setNodeExpression nid update

localSetNodeExpression :: NodeId -> Text -> Command State Bool
localSetNodeExpression nid update = do
    modifyExpressionNode nid $ expression .= update
    inGraph nid
