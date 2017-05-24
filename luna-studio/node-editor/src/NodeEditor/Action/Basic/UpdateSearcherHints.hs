module NodeEditor.Action.Basic.UpdateSearcherHints where

import           Common.Prelude
import qualified Data.Map.Lazy                      as Map
import qualified Data.Text                          as Text
import           LunaStudio.Data.Node               (ExpressionNode)
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.NodeEditor (getNodeSearcherData, modifySearcher)
import           NodeEditor.Batch.Workspace         (nodeSearcherData)
import           NodeEditor.React.Model.Searcher    (Mode, allCommands, updateCommandsResult, updateNodeResult)
import qualified NodeEditor.React.Model.Searcher    as Searcher
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
    modifySearcher $ do
        mayQuery <- preuse $ Searcher.input . Searcher._Divided
        m        <- use Searcher.mode
        let (mode, hintsLen) = case m of
                Searcher.Node {}    -> do
                    let getHints q = if Text.null $ q ^. Searcher.prefix
                            then filterNodesData m nodesData
                            else nodesData
                        items' = maybe [] (\q -> searchInScope (getHints q) (q ^. Searcher.query)) mayQuery
                    (updateNodeResult items' m, length items')
                Searcher.Command {} -> do
                    let items' = maybe [] (searchInScope allCommands . view Searcher.query) mayQuery
                    (updateCommandsResult items' m, length items')
                _                   -> (m, 0)
        Searcher.selected      .= min 1 hintsLen
        Searcher.rollbackReady .= False
        Searcher.mode          .= mode

filterNodesData :: Mode -> Items ExpressionNode -> Items ExpressionNode
filterNodesData (Searcher.Node _ (Just typeRep) _ _) =
    Map.filterWithKey filterFunction where
        filterFunction k a = isElement a || k == (convert $ toString typeRep)
filterNodesData _ = Map.filter isElement
