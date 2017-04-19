--TODO[LJK, PM]: Review names in this module
module Luna.Studio.Action.Basic.SetNodeMode where

import           Luna.Studio.Action.Basic.Merge              (localUnmerge)
import           Luna.Studio.Action.Basic.UpdateNode         (updatePortSelfVisibilityForIds)
import qualified Luna.Studio.Action.Batch                    as Batch
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getSelectedNodes)
import qualified Luna.Studio.Action.State.NodeEditor         as NodeEditor
import           Luna.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, Mode, isExpandedFunction, isMode, mode, nodeLoc)
import           Luna.Studio.State.Global                    (State)


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
