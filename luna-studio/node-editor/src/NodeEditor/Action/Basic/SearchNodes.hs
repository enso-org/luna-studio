module NodeEditor.Action.Basic.SearchNodes where

import           Common.Prelude
import           LunaStudio.Data.Node               (ExpressionNode)
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.NodeEditor (getNodeSearcherData, modifySearcher)
import           NodeEditor.Batch.Workspace         (nodeSearcherData)
import           NodeEditor.React.Model.Searcher    (input, isNode, mode, rollbackReady, selected, updateNodeResult)
import           NodeEditor.State.Global            (State, workspace)
import           Text.ScopeSearcher.Item            (Items)
import           Text.ScopeSearcher.Scope           (searchInScope)


localSetSearcherHints :: Items ExpressionNode -> Command State ()
localSetSearcherHints items' = do
    workspace . _Just . nodeSearcherData .= items'
    nodesData' <- getNodeSearcherData
    modifySearcher $ whenM (use isNode) $ do
        query    <- use input
        let items'' = searchInScope nodesData' query
        selected      .= min 1 (length items'')
        rollbackReady .= False
        mode          %= updateNodeResult items''
