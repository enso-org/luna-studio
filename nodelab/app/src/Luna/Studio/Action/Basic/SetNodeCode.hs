module Luna.Studio.Action.Basic.SetNodeCode where

import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node        (NodeId)
import qualified Luna.Studio.React.Model.Node        as Node
import           Luna.Studio.State.Global            (State)


setNodeCode :: NodeId -> Text -> Command State ()
setNodeCode nid code =
    whenM (localSetNodeCode nid code) $ Batch.setNodeCode nid code

localSetNodeCode :: NodeId -> Text -> Command State Bool
localSetNodeCode nid code = do
    NodeEditor.modifyNode nid $ Node.code .= Just code
    NodeEditor.inNodeEditor nid
