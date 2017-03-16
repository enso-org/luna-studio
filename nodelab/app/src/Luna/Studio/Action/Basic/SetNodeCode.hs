module Luna.Studio.Action.Basic.SetNodeCode where

import           Empire.API.Data.Node                (NodeId)
import qualified Empire.API.Data.Node                as Node
import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.Graph      (inGraph)
import qualified Luna.Studio.Action.State.Graph      as Graph
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node        as Model
import           Luna.Studio.State.Global            (State)


setNodeCode :: NodeId -> Text -> Command State ()
setNodeCode nid code =
    whenM (localSetNodeCode nid code) $ Batch.setNodeCode nid code

localSetNodeCode :: NodeId -> Text -> Command State Bool
localSetNodeCode nid code = do
    Graph.modifyNode      nid $ Node.code .~ Just code
    NodeEditor.modifyNode nid $ Model.code .= Just code
    inGraph nid
