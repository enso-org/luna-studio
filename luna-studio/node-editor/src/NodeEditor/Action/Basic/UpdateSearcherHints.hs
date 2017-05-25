module NodeEditor.Action.Basic.UpdateSearcherHints where

import           Common.Prelude
import           Control.Monad.Extra                (mapMaybeM)
import qualified Control.Monad.State.Lazy           as S
import           Data.Map.Lazy                      (Map)
import qualified Data.Map.Lazy                      as Map
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           LunaStudio.Data.Node               (ExpressionNode)
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.NodeEditor (getNodeSearcherData, modifySearcher)
import           NodeEditor.Batch.Workspace         (nodeSearcherData)
import           NodeEditor.React.Model.Searcher    (ClassName, allCommands, updateCommandsResult, updateNodeResult)
import qualified NodeEditor.React.Model.Searcher    as Searcher
import           NodeEditor.State.Global            (State, workspace)
import           Text.ScopeSearcher.Item            (Items, isElement, isGroup)
import           Text.ScopeSearcher.QueryResult     (QueryResult)
import qualified Text.ScopeSearcher.QueryResult     as Result
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
                (Searcher.Node _ cn _ _) -> do
                    let cn' q = if Text.null . Text.dropWhile (== ' ') $ q ^. Searcher.prefix then cn else def
                        items' = mergeByName $ maybe [] (\q -> getHintsForNode (q ^. Searcher.query) (cn' q) nsData) mayQuery
                    (updateNodeResult items' m, length items')
                Searcher.Command {} -> do
                    let items' = maybe [] (searchInScope allCommands . view Searcher.query) mayQuery
                    (updateCommandsResult items' m, length items')
                _                   -> (m, 0)
        Searcher.selected      .= min 1 hintsLen
        Searcher.rollbackReady .= False
        Searcher.mode          .= mode

getHintsForNode :: Text -> Maybe ClassName -> Items ExpressionNode -> [QueryResult ExpressionNode]
getHintsForNode query Nothing   nsData = searchInScope (globalFunctions nsData) query
                                      <> searchInScope (allMethods nsData) query
getHintsForNode query (Just cn) nsData = searchInScope (methodsForClass cn nsData) query
                                      <> searchInScope (globalFunctions nsData) query
                                      <> searchInScope (allMethodsWithoutClass cn nsData) query

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

mergeByName :: [QueryResult a] -> [QueryResult a]
mergeByName results = S.evalState (mapMaybeM processResult results) prefixesMap where
    prefixesMap :: Map Text [Text]
    prefixesMap = foldl (\m a -> Map.insertWith mapInsertFunction (a ^. Result.name) [a ^. Result.prefix] m) Map.empty results
    mapInsertFunction :: [Text] -> [Text] -> [Text]
    mapInsertFunction newV oldV = if      length oldV > 3  then oldV
                                  else if length oldV == 3 then oldV <> [convert "..."]
                                                           else oldV <> newV
    processResult :: QueryResult a -> S.State (Map Text [Text]) (Maybe (QueryResult a))
    processResult res = (S.gets $ Map.lookup (res ^. Result.name)) >>= \case
        Nothing       -> return Nothing
        Just prefixes -> do
            S.modify $ Map.delete (res ^. Result.name)
            return . Just $ res & Result.prefix .~ mergePrefixes prefixes
    mergePrefixes :: [Text] -> Text
    mergePrefixes [t]      = t
    mergePrefixes prefixes = Text.intercalate (convert ", ") $ map (\p -> if Text.null p then convert "Function" else p) prefixes