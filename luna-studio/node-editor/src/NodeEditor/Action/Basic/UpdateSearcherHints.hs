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
import           NodeEditor.Action.State.NodeEditor (getLocalFunctions, getNodeSearcherData, modifySearcher)
import           NodeEditor.Batch.Workspace         (nodeSearcherData)
import           NodeEditor.React.Model.Searcher    (allCommands, className, updateCommandsResult, updateNodeResult)
import qualified NodeEditor.React.Model.Searcher    as Searcher
import           NodeEditor.State.Global            (State, workspace)
import           Text.ScopeSearcher.Item            (Items, isElement, isGroup)
import           Text.ScopeSearcher.QueryResult     (QueryResult)
import qualified Text.ScopeSearcher.QueryResult     as Result
import           Text.ScopeSearcher.Scope           (searchInScope)

type IsFirstQuery = Bool

localSetSearcherHints :: Items ExpressionNode -> Command State ()
localSetSearcherHints items' = do
    workspace . _Just . nodeSearcherData .= items'
    localUpdateSearcherHints

localUpdateSearcherHints :: Command State ()
localUpdateSearcherHints = do
    nsData         <- getNodeSearcherData
    localFunctions <- getLocalFunctions
    modifySearcher $ do
        mayQuery <- preuse $ Searcher.input . Searcher._Divided
        m        <- use Searcher.mode
        let (mode, hintsLen) = case m of
                (Searcher.Node _ nmi _) -> do
                    let isFirstQuery q = Text.null . Text.dropWhile (== ' ') $ q ^. Searcher.prefix
                        items' = mergeByName $ maybe [] (\q -> getHintsForNode (q ^. Searcher.query) (nmi ^. className) nsData localFunctions (isFirstQuery q)) mayQuery
                    (updateNodeResult items' m, length items')
                Searcher.Command {} -> do
                    let items' = maybe [] (searchInScope allCommands . view Searcher.query) mayQuery
                    (updateCommandsResult items' m, length items')
                _                   -> (m, 0)
        Searcher.selected      .= min 1 hintsLen
        Searcher.rollbackReady .= False
        Searcher.mode          .= mode

getHintsForNode :: Text -> Maybe Text -> Items ExpressionNode -> Items ExpressionNode -> IsFirstQuery -> [QueryResult ExpressionNode]
getHintsForNode query _         nsData localFunctions False = (setPrefix (convert "Local Function")  $ searchInScope localFunctions query)
                                                           <> (setPrefix (convert "Global Function") $ searchInScope (globalFunctions nsData) query)
                                                           <> searchInScope (allMethods nsData) query
getHintsForNode query Nothing   nsData localFunctions True  = (setPrefix (convert "Global Function") $ searchInScope (globalFunctions nsData) query)
                                                           <> (setPrefix (convert "Local Function")  $ searchInScope localFunctions query)
                                                           <> searchInScope (allMethods nsData) query
getHintsForNode query (Just cn) nsData localFunctions True  = searchInScope (methodsForClass cn nsData) query
                                                           <> (setPrefix (convert "Global Function") $ searchInScope (globalFunctions nsData) query)
                                                           <> (setPrefix (convert "Local Function")  $ searchInScope localFunctions query)
                                                           <> searchInScope (allMethodsWithoutClass cn nsData) query

globalFunctions :: Items ExpressionNode -> Items ExpressionNode
globalFunctions = Map.filter isElement

methodsForClass :: Text -> Items ExpressionNode -> Items ExpressionNode
methodsForClass className' = Map.filterWithKey filterFunction where
    filterFunction k a = isGroup a && k == className'

allMethods :: Items ExpressionNode -> Items ExpressionNode
allMethods = Map.filter isGroup

allMethodsWithoutClass :: Text -> Items ExpressionNode -> Items ExpressionNode
allMethodsWithoutClass className' = Map.filterWithKey filterFunction where
    filterFunction k a = isGroup a && k /= className'

setPrefix :: Text -> [QueryResult a] -> [QueryResult a]
setPrefix prefix = map (\r -> r & Result.prefix .~ prefix)

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
    mergePrefixes prefixes = Text.intercalate (convert ", ") $ map (\p -> if Text.null p then convert "Class" else p) prefixes
