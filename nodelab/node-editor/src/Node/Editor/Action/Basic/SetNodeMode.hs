--TODO[LJK, PM]: Review names in this module
module Node.Editor.Action.Basic.SetNodeMode where

import           Node.Editor.Action.Basic.Merge              (localUnmerge)
import           Node.Editor.Action.Basic.UpdateNode         (updatePortSelfVisibilityForIds)
import qualified Node.Editor.Action.Batch                    as Batch
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.State.NodeEditor         (getSelectedNodes)
import qualified Node.Editor.Action.State.NodeEditor         as NodeEditor
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (ExpressionNode, Mode, isExpandedFunction, isMode, mode, nodeLoc)
import           Node.Editor.State.Global                    (State)


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
    void $ updatePortSelfVisibilityForIds nodeLocs
