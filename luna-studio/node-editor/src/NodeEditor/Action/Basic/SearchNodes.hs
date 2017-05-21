module NodeEditor.Action.Basic.SearchNodes where

import           Common.Prelude
import           Data.Map.Lazy                      as Map
import           LunaStudio.Data.Node               (ExpressionNode)
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.NodeEditor (getNodeSearcherData, modifySearcher)
import           NodeEditor.Batch.Workspace         (nodeSearcherData)
import           NodeEditor.React.Model.Searcher    (Mode (Node), NewNode (NewNode), input, isNode, mode, rollbackReady, selected,
                                                     updateNodeResult)
import           NodeEditor.State.Global            (State, workspace)
import           Text.ScopeSearcher.Item            (Items, isElement)
import           Text.ScopeSearcher.Scope           (searchInScope)


localSetSearcherHints :: Items ExpressionNode -> Command State ()
localSetSearcherHints items' = do
    workspace . _Just . nodeSearcherData .= items'
    localUpdateSearcherHints

localUpdateSearcherHints :: Command State ()
localUpdateSearcherHints = do
    nodesData <- getNodeSearcherData
    modifySearcher $ whenM (use isNode) $ do
        query <- use input
        mode' <- use mode
        let items'' = searchInScope (filterNodesData mode' nodesData) query
        selected      .= min 1 (length items'')
        rollbackReady .= False
        mode          %= updateNodeResult items''

filterNodesData :: Mode -> Items ExpressionNode -> Items ExpressionNode
filterNodesData (Node _ (Just (NewNode _ (Just (_, typeRep)))) _) = Map.filterWithKey filterFunction where
    filterFunction k a = isElement a || k  == (convert $ toString typeRep)
filterNodesData _ = Map.filter isElement
