module Luna.Studio.Action.Basic.RenameNode where

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


renameNode :: NodeId -> Text -> Command State ()
renameNode nid update =
    whenM (localRenameNode nid update) $ Batch.renameNode nid update

localRenameNode :: NodeId -> Text -> Command State Bool
localRenameNode nid update = do
    Graph.modifyNode      nid $ \node -> node & Node.name .~ update
    NodeEditor.modifyNode nid $ do
        Model.name     .= update
        Model.nameEdit .= Nothing
    inGraph nid
