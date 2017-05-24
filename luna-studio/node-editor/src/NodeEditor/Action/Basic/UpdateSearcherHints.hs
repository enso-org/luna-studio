module NodeEditor.Action.Basic.UpdateSearcherHints where

import           Common.Prelude
import qualified Data.Map.Lazy                      as Map
import qualified Data.Text                          as Text
import           LunaStudio.Data.Node               (ExpressionNode)
import           LunaStudio.Data.TypeRep            (TypeRep)
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.NodeEditor (getNodeSearcherData, modifySearcher)
import           NodeEditor.Batch.Workspace         (nodeSearcherData)
import           NodeEditor.React.Model.Searcher    (allCommands, updateCommandsResult, updateNodeResult)
import qualified NodeEditor.React.Model.Searcher    as Searcher
import           NodeEditor.State.Global            (State, workspace)
import           Text.ScopeSearcher.Item            (Items, isElement, isGroup)
import           Text.ScopeSearcher.QueryResult     (QueryResult)
import           Text.ScopeSearcher.Scope           (searchInScope)


localSetSearcherHints :: Items ExpressionNode -> Command State ()
localSetSearcherHints items' = do
    workspace . _Just . nodeSearcherData .= items'
    localUpdateSearcherHints

localUpdateSearcherHints :: Command State ()
localUpdateSearcherHints = do
    nsData <- getNodeSearcherData
    modifySearcher $ do
        mayQuery <- preuse $ Searcher.input . Searcher._Divided
        m        <- use Searcher.mode
        let (mode, hintsLen) = case m of
                (Searcher.Node _ tpe _ _) -> do
                    let tpe' q = if Text.null . Text.dropWhile (== ' ') $ q ^. Searcher.prefix then tpe else def
                        items' = maybe [] (\q -> getHintsForNode (q ^. Searcher.query) (tpe' q) nsData) mayQuery
                    (updateNodeResult items' m, length items')
                Searcher.Command {} -> do
                    let items' = maybe [] (searchInScope allCommands . view Searcher.query) mayQuery
                    (updateCommandsResult items' m, length items')
                _                   -> (m, 0)
        Searcher.selected      .= min 1 hintsLen
        Searcher.rollbackReady .= False
        Searcher.mode          .= mode

getHintsForNode :: Text -> Maybe TypeRep -> Items ExpressionNode -> [QueryResult ExpressionNode]
getHintsForNode query Nothing    nsData = searchInScope (globalFunctions nsData) query
                                       <> searchInScope (allMethods nsData) query
getHintsForNode query (Just tpe) nsData = searchInScope (methodsForClass (convert $ toString tpe) nsData) query
                                       <> searchInScope (globalFunctions nsData) query
                                       <> searchInScope (allMethodsWithoutClass (convert $ toString tpe) nsData) query


globalFunctions :: Items ExpressionNode -> Items ExpressionNode
globalFunctions = Map.filter isElement

methodsForClass :: Text -> Items ExpressionNode -> Items ExpressionNode
methodsForClass className = Map.filterWithKey filterFunction where
    filterFunction k a = isGroup a && k == className

allMethods :: Items ExpressionNode -> Items ExpressionNode
allMethods = Map.filter isGroup

allMethodsWithoutClass :: Text -> Items ExpressionNode -> Items ExpressionNode
allMethodsWithoutClass className = Map.filterWithKey filterFunction where
    filterFunction k a = isGroup a && k /= className
