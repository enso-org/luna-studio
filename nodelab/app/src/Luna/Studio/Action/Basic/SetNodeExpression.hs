module Luna.Studio.Action.Basic.SetNodeExpression where

import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.NodeEditor (inGraph, modifyNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node        (NodeId, expression)
import           Luna.Studio.State.Global            (State)


setNodeExpression :: NodeId -> Text -> Command State ()
setNodeExpression nid update =
    whenM (localSetNodeExpression nid update) $ Batch.setNodeExpression nid update

localSetNodeExpression :: NodeId -> Text -> Command State Bool
localSetNodeExpression nid update = do
    exists <- inGraph nid
    when exists $ modifyNode nid $ expression .= update
    return exists
