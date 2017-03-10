module Luna.Studio.Action.Basic.SetNodeMode where

import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNodes)
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.State.NodeEditor     (getSelectedNodes)
import           Luna.Studio.Action.State.NodeEditor     (getNode)
import qualified Luna.Studio.Action.State.NodeEditor     as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node            (Mode, NodeId, isMode, mode, nodeId)
import           Luna.Studio.State.Global                (State)


setSelectedNodesMode :: Mode -> Command State ()
setSelectedNodesMode newMode = do
    selectedNodes <- getSelectedNodes
    let allNewMode   = all (isMode newMode) selectedNodes
        updatedNodes = flip map selectedNodes $ \node ->
            node & mode .~ if allNewMode then def else newMode
    mapM_ NodeEditor.addNode updatedNodes
    void . redrawConnectionsForNodes $ map (view nodeId) updatedNodes

setNodeMode :: NodeId -> Mode -> Command State Bool
setNodeMode node = fmap (not . null) . setNodesMode [node]

setNodesMode :: [NodeId] -> Mode -> Command State [NodeId]
setNodesMode nids newMode = do
    update <- map (\node -> node & mode .~ newMode) . catMaybes <$> mapM getNode nids
    mapM_ NodeEditor.addNode update
    let updatedNids = map (view nodeId) update
    void $ redrawConnectionsForNodes updatedNids
    return updatedNids
