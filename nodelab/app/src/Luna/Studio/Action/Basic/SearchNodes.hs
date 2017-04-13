module Luna.Studio.Action.Basic.SearchNodes where

import           Empire.API.Data.Node                (ExpressionNode)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.State.NodeEditor (getNodeSearcherData, modifySearcher)
import           Luna.Studio.Batch.Workspace         (nodeSearcherData)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Searcher    (Mode (Node), input, isNode, mode, rollbackReady, selected)
import           Luna.Studio.State.Global            (State, workspace)
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
