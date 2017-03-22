module Luna.Studio.Action.Basic.SetNodeCode where

import qualified Luna.Studio.Action.Batch                    as Batch
import           Luna.Studio.Action.Command                  (Command)
import qualified Luna.Studio.Action.State.NodeEditor         as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeId, code)
import           Luna.Studio.State.Global                    (State)


setNodeCode :: NodeId -> Text -> Command State ()
setNodeCode nid update =
    whenM (localSetNodeCode nid update) $ Batch.setNodeCode nid update

localSetNodeCode :: NodeId -> Text -> Command State Bool
localSetNodeCode nid update = do
    NodeEditor.modifyExpressionNode nid $ code .= Just update
    NodeEditor.inGraph nid
