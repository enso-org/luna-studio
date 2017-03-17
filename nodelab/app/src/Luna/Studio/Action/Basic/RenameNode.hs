module Luna.Studio.Action.Basic.RenameNode where

import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node        (NodeId)
import qualified Luna.Studio.React.Model.Node        as Node
import           Luna.Studio.State.Global            (State)


renameNode :: NodeId -> Text -> Command State ()
renameNode nid update =
    whenM (localRenameNode nid update) $ Batch.renameNode nid update

localRenameNode :: NodeId -> Text -> Command State Bool
localRenameNode nid update = do
    NodeEditor.modifyNode nid $ do
        Node.name     .= update
        Node.nameEdit .= Nothing
    NodeEditor.inGraph nid
