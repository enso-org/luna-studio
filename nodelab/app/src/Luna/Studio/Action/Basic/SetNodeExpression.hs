module Luna.Studio.Action.Basic.SetNodeExpression where

import qualified Empire.API.Data.Node                as Empire
import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import qualified Luna.Studio.Action.State.Graph      as Graph
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node        (NodeId, NodeType (ExpressionNode), nodeType)
import           Luna.Studio.State.Global            (State)

setNodeExpression :: NodeId -> Text -> Command State ()
setNodeExpression nid update =
    whenM (localSetNodeExpression nid update) $ Batch.setNodeExpression nid update

localSetNodeExpression :: NodeId -> Text -> Command State Bool
localSetNodeExpression nid update = do
    mayType <- (fmap . fmap) (view nodeType) $ NodeEditor.getNode nid
    case mayType of
        Just (ExpressionNode _) -> do
            Graph.modifyNode      nid $ Empire.nodeType .~ ExpressionNode update
            NodeEditor.modifyNode nid $ nodeType .= ExpressionNode update
            return True
        _                   -> return False
