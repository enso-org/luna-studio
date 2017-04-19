module Node.Editor.Action.Basic.SearchNodes where

import           Empire.API.Data.Node                (ExpressionNode)
import           Node.Editor.Action.Command          (Command)
import           Node.Editor.Action.State.NodeEditor (getNodeSearcherData, modifySearcher)
import           Node.Editor.Batch.Workspace         (nodeSearcherData)
import           Luna.Prelude
import           Node.Editor.React.Model.Searcher    (Mode (Node), input, isNode, mode, rollbackReady, selected)
import           Node.Editor.State.Global            (State, workspace)
import           Text.ScopeSearcher.Item             (Items)
import           Text.ScopeSearcher.Scope            (searchInScope)


localSetSearcherHints :: Items ExpressionNode -> Command State ()
localSetSearcherHints items' = do
    workspace . nodeSearcherData .= items'
    nodesData' <- getNodeSearcherData
    modifySearcher $ whenM (use isNode) $ do
        query    <- use input
        let items'' = searchInScope nodesData' query
        selected      .= min 1 (length items'')
        rollbackReady .= False
        mode          .= Node items''
