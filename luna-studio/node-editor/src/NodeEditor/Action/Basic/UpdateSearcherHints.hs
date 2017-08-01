module NodeEditor.Action.Basic.UpdateSearcherHints where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           Control.Monad.Extra                (mapMaybeM)
import qualified Control.Monad.State.Lazy           as S
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           LunaStudio.Data.Node               (ExpressionNode)
import qualified LunaStudio.Data.NodeSearcher       as NS
import           NodeEditor.Action.State.NodeEditor (getLocalFunctions, getNodeSearcherData, modifySearcher)
import           NodeEditor.React.Model.Searcher    (allCommands, className, updateCommandsResult, updateNodeResult)
import qualified NodeEditor.React.Model.Searcher    as Searcher
import           NodeEditor.State.Global            (State, nodeSearcherData)
import           Text.ScopeSearcher.Item            (Items, isElement, isGroup)
import           Text.ScopeSearcher.QueryResult     (QueryResult)
import qualified Text.ScopeSearcher.QueryResult     as Result
import           Text.ScopeSearcher.Scope           (searchInScope)

type IsFirstQuery = Bool

localSetSearcherHints :: Items ExpressionNode -> Command State ()
localSetSearcherHints items' = do
    nodeSearcherData .= items'
    localUpdateSearcherHints

localUpdateSearcherHints :: Command State ()
localUpdateSearcherHints = do
    nsData         <- getNodeSearcherData
    localFunctions <- getLocalFunctions
    modifySearcher $ do
        mayQuery <- preuse $ Searcher.input . Searcher._Divided
        m        <- use Searcher.mode
        let selectInput      = maybe True (Text.null . view Searcher.query) mayQuery
            (mode, hintsLen) = case m of
                (Searcher.Node _ nmi _) -> do
                    let isFirstQuery         q    = Text.null . Text.dropWhile (== ' ') $ q ^. Searcher.prefix
                        strippedPrefix       q    = Text.dropWhileEnd (== ' ') $ q ^. Searcher.prefix
                        searchForMethodsOnly q    = if Text.null $ strippedPrefix q then False else Text.last (strippedPrefix q) == '.'
                        filterNSData        nsd q = if searchForMethodsOnly q then allMethods nsd else nsd
                        filterLocalFuntions lfd q = if searchForMethodsOnly q then def else lfd
                        items' = mergeByName . flip (maybe []) mayQuery $ \q ->
                            getHintsForNode (q ^. Searcher.query)
                                            (nmi ^. className)
                                            (filterNSData nsData q)
                                            (filterLocalFuntions localFunctions q)
                                            (isFirstQuery q)
                    (updateNodeResult items' m, length items')
                Searcher.Command {} -> do
                    let items' = maybe [] (searchInScope allCommands . view Searcher.query) mayQuery
                    (updateCommandsResult items' m, length items')
                _                   -> (m, 0)
        Searcher.selected      .= if selectInput then 0 else min 1 hintsLen
        Searcher.rollbackReady .= False
        Searcher.mode          .= mode

localClearSearcherHints :: Command State ()
localClearSearcherHints = modifySearcher $ do
    Searcher.selected      .= def
    Searcher.rollbackReady .= False
    Searcher.mode          %= \case
        Searcher.Command         _ -> Searcher.Command def
        Searcher.Node     nl nmi _ -> Searcher.Node nl nmi def
        Searcher.NodeName nl     _ -> Searcher.NodeName nl def
        Searcher.PortName pr     _ -> Searcher.PortName pr def


getHintsForNode :: Text -> Maybe Text -> Items ExpressionNode -> Items ExpressionNode -> IsFirstQuery -> [QueryResult ExpressionNode]
getHintsForNode q cn nsData localFunctions isFirst = if convert q == "_"
    then searchInScope (Map.fromList [NS.entry q]) q <> getHintsForNode' q cn nsData localFunctions isFirst
    else getHintsForNode' q cn nsData localFunctions isFirst

getHintsForNode' :: Text -> Maybe Text -> Items ExpressionNode -> Items ExpressionNode -> IsFirstQuery -> [QueryResult ExpressionNode]
getHintsForNode' query _         nsData localFunctions False = searchInScope localFunctions query
                                                           <> searchInScope (globalFunctions nsData) query
                                                           <> searchInScope (allMethods nsData) query
getHintsForNode' query Nothing   nsData localFunctions True  = searchInScope (globalFunctions nsData) query
                                                           <> searchInScope localFunctions query
                                                           <> searchInScope (allMethods nsData) query
getHintsForNode' query (Just cn) nsData localFunctions True  = searchInScope (methodsForClass cn nsData) query
                                                           <> searchInScope (globalFunctions nsData) query
                                                           <> searchInScope localFunctions query
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

type IsFunction = Bool

mergeByName :: [QueryResult a] -> [QueryResult a]
mergeByName results = S.evalState (mapMaybeM processResult results) methodMapAndFunctionSet where
    methodMapAndFunctionSet :: (Map Text [Text], Set Text)
    methodMapAndFunctionSet = foldl foldFunction (Map.empty, Set.empty) results
    foldFunction :: (Map Text [Text], Set Text) -> QueryResult a -> (Map Text [Text], Set Text)
    foldFunction (m,s) v = if Text.null $ v ^. Result.prefix
        then (m, Set.insert (v ^. Result.name) s)
        else (Map.insertWith mapInsertFunction (v ^. Result.name) [v ^. Result.prefix] m, s)
    mapInsertFunction :: [Text] -> [Text] -> [Text]
    mapInsertFunction newV oldV = if      length oldV > 3  then oldV
                                  else if length oldV == 3 then oldV <> [convert "..."]
                                                           else oldV <> newV

    processResult :: QueryResult a -> S.State (Map Text [Text], Set Text) (Maybe (QueryResult a))
    processResult res = if Text.null $ res ^. Result.prefix then processFunction res else processMethod res
    processFunction :: QueryResult a -> S.State (Map Text [Text], Set Text) (Maybe (QueryResult a))
    processFunction res = (S.gets $ Set.member (res ^. Result.name) . (view _2)) >>= \inSet -> if not inSet then return Nothing else do
        _2 %= Set.delete (res ^. Result.name)
        return $ Just res
    processMethod :: QueryResult a -> S.State (Map Text [Text], Set Text) (Maybe (QueryResult a))
    processMethod res = (S.gets $ Map.lookup (res ^. Result.name) . (view _1)) >>= \case
        Nothing       -> return Nothing
        Just prefixes -> do
            _1 %= Map.delete (res ^. Result.name)
            return . Just $ res & Result.prefix .~ Text.intercalate (convert ", ") prefixes
