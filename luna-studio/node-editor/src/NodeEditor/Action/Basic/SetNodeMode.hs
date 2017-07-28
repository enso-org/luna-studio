--TODO[LJK, PM]: Review names in this module
module NodeEditor.Action.Basic.SetNodeMode where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           NodeEditor.Action.Basic.Merge              (localUnmerge)
import qualified NodeEditor.Action.Batch                    as Batch
import           NodeEditor.Action.State.Model              (updatePortsModeForNode)
import           NodeEditor.Action.State.NodeEditor         (getSelectedNodes)
import qualified NodeEditor.Action.State.NodeEditor         as NodeEditor
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, Mode, isExpandedFunction, isMode, mode, nodeLoc)
import           NodeEditor.State.Global                    (State)


toggleSelectedNodesMode :: Mode -> Command State ()
toggleSelectedNodesMode newMode = do
    nodes <- getSelectedNodes
    let allNewMode = all (isMode newMode) nodes
    toggleNodesMode allNewMode newMode nodes

toggleSelectedNodesUnfold :: Command State ()
toggleSelectedNodesUnfold = do
    nodes <- getSelectedNodes
    let allNewMode = all isExpandedFunction nodes
    if allNewMode then do
        mapM_ localUnmerge nodes
        toggleNodesMode allNewMode def nodes
    else
        mapM_ (Batch.getSubgraph . (view nodeLoc)) nodes

toggleNodesMode :: Bool -> Mode -> [ExpressionNode] -> Command State ()
toggleNodesMode allNewMode newMode nodes = do
    updatedNodes <- forM nodes $ \node -> do
        when (isExpandedFunction node) $ localUnmerge node
        return $ node & mode .~ if allNewMode then def else newMode
    let nodeLocs = map (view nodeLoc) updatedNodes
    forM_ updatedNodes $ \node -> NodeEditor.addExpressionNode node
    mapM_ updatePortsModeForNode nodeLocs
