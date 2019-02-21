module NodeEditor.Action.Basic.UpdateSearcherHints where

import Common.Prelude

import qualified Data.Aeson                                   as Aeson
import qualified Data.ByteString.Lazy.Char8                   as BS
import qualified Data.JSString                                as JSString
import qualified Data.Map                                     as Map
import qualified Data.Set                                     as Set
import qualified Data.Text                                    as Text
import qualified IdentityString                               as IS
import qualified LunaStudio.Data.Searcher.Hint                as Hint
import qualified LunaStudio.Data.Searcher.Hint.Library        as Library
{-import qualified LunaStudio.Data.Searcher.Hint.Node           as NodeHint-}
import qualified NodeEditor.React.Model.Searcher              as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint.Node    as NodeHint
import qualified NodeEditor.React.Model.Searcher.Hint         as Hint
import qualified NodeEditor.React.Model.Searcher.Input        as Input
{-import qualified NodeEditor.React.Model.Searcher.Mode.Command as CommandSearcher-}
import qualified NodeEditor.React.Model.Searcher.Mode.Node    as NodeSearcher
import qualified NodeEditor.React.Model.Visualization         as Visualization
import qualified NodeEditor.State.Global                      as Global
import qualified Searcher.Engine.Data.Match                   as Match
import qualified Searcher.Engine.Data.Result                  as Result
import qualified Searcher.Engine.Search                       as Search

import Common.Action.Command              (Command)
import Data.Map                           (Map)
import Data.Set                           (Set)
import Data.Text                          (Text)
import JS.Visualizers                     (sendVisualizationData)
import LunaStudio.Data.TypeRep            (ConstructorRep (ConstructorRep))
import NodeEditor.Action.Batch            (searchNodes)
import NodeEditor.Action.State.NodeEditor (getLocalFunctions, getSearcher,
                                           inTopLevelBreadcrumb, modifySearcher)
import NodeEditor.React.Model.Searcher    (Searcher)
{-import NodeEditor.React.Model.Searcher    (ClassName, LibrariesHintsMap,-}
                                           {-LibraryName, Match, NodeSearcherData,-}
                                           {-Searcher, Symbol, TypePreference,-}
                                           {-allCommands,-}
                                           {-localFunctionsLibraryName)-}
import LunaStudio.Data.Searcher.Hint.Library (SearcherLibraries)
import NodeEditor.State.Global            (State)
import Searcher.Engine.Data.Result        (Result)
import Searcher.Engine.Metric.DefaultMetric (DefaultMetric)


type IsFirstQuery         = Bool
type SearchForMethodsOnly = Bool

positionSucc :: Maybe Int -> Maybe Int
positionSucc = \case
    Just i  -> Just $ i + 1
    Nothing -> Just 0

positionPred :: Maybe Int -> Maybe Int
positionPred = \case
    Nothing -> Nothing
    Just 0  -> Nothing
    Just i  -> Just $ i - 1

selectNextHint :: Command State ()
selectNextHint = modifySearcher $ do
    hintsLen <- use (Searcher.results . to length)
    Searcher.selectedPosition %= fmap (min hintsLen) . positionPred

selectPreviousHint :: Command State ()
selectPreviousHint = modifySearcher $ Searcher.selectedPosition %= positionPred

selectHint :: Maybe Int -> Command State ()
selectHint i = modifySearcher $ do
    hLen <- fmap Just $ use $ Searcher.results . to length
    when (i <= hLen) $ Searcher.selectedPosition .= i

localAddSearcherHints :: SearcherLibraries -> Command State ()
localAddSearcherHints libHints = do
    Global.searcherDatabase %= NodeHint.insertSearcherLibraries libHints
    localUpdateSearcherHintsPreservingSelection

setImportedLibraries :: Set Library.Name -> Command State ()
setImportedLibraries libs = do
    Global.searcherDatabase . NodeHint.imported .= libs
    missingLibs <- use $ Global.searcherDatabase . NodeHint.missingLibraries
    unless (null missingLibs) $ do
        searchNodes missingLibs

updateDocumentation :: Command State ()
updateDocumentation = withJustM getSearcher $ \s -> do
    let mayDocVis = s ^? Searcher.documentationVisualization
        mayDoc = s ^? Searcher.selectedResult . _Just . Hint.documentation
        mayDocData = (,) <$> mayDocVis <*> mayDoc
    withJust mayDocData $ \(docVis, doc) -> liftIO $ sendVisualizationData
        (docVis ^. Visualization.visualizationId)
        (ConstructorRep "Text" def)
        =<< (IS.fromJSString . JSString.pack . BS.unpack $ Aeson.encode doc)

localUpdateSearcherHintsPreservingSelection :: Command State ()
localUpdateSearcherHintsPreservingSelection = localUpdateSearcherHints -- return () -- do
    {-maySelected <- maybe def (view Searcher.selectedHint) <$> getSearcher-}
    {-localUpdateSearcherHints'-}
    {-withJust maySelected $ \selected -> do-}
        {-let equalsType (Searcher.CommandHint _) (Searcher.CommandHint _) = True-}
            {-equalsType (Searcher.NodeHint    _) (Searcher.NodeHint    _) = True-}
            {-equalsType _                       _                         = False-}
            {-equalsName h1 h2 = h1 ^. Match.name == h2 ^. Match.name-}
            {-equals h1 h2 = equalsType h1 h2 && equalsName h1 h2-}
        {-hints <- maybe def (view Searcher.hints) <$> getSearcher-}
        {-withJust (findIndex (equals selected) hints) $ selectHint . (+1)-}
    {-updateDocumentation-}

localUpdateSearcherHints :: Command State ()
localUpdateSearcherHints = localUpdateSearcherHints' >> updateDocumentation

localUpdateSearcherHints' :: Command State ()
localUpdateSearcherHints' = do -- return () -- unlessM inTopLevelBreadcrumb $ do
    nsData        <- use Global.searcherDatabase
    {-localFunctions <- getLocalFunctions-}
    {-let nsData :: NodeSearcherData-}
        {-nsData = nsData'-}
            {-& Searcher.libraries %~ Map.insert-}
                {-localFunctionsLibraryName-}
                {-(Searcher.mkLocalFunctionsLibrary localFunctions)-}
            {-& Searcher.importedLibraries %~ Set.insert localFunctionsLibraryName-}
    modifySearcher $ do
        mayQuery <- preuse $ Searcher.input . Input._DividedInput
        let query = fromMaybe def mayQuery
            results = search query nsData Nothing
        Searcher.results .= results
        {-let updateCommands s = do-}
                {-let hints input = CommandSearcher.search-}
                        {-(input ^. Input.query)-}
                        {-allCommands-}
                {-maybe mempty hints mayQuery-}
            {-updateNodeSearcher s = do-}
                {-let mayClassName = s ^? Searcher.modeData-}
                        {-. Searcher._ExpressionMode . Searcher.className . _Just-}
                    {-hints input-}
                        {-= if has (Searcher.modeData . Searcher._ExpressionMode) s-}
                            {-then search input nsData mayClassName-}
                            {-else mempty-}
                {-s & Searcher.nodes .~ maybe mempty hints mayQuery-}
            {-updateMode (Searcher.CommandSearcher s)-}
                {-= Searcher.CommandSearcher $ updateCommands s-}
            {-updateMode (Searcher.NodeSearcher s)-}
                {-= Searcher.NodeSearcher $ updateNodeSearcher s-}
            {-selectInput = maybe True (Text.null . view Input.query) mayQuery-}
        {-Searcher.mode          %= updateMode-}
        {-hintsLen <- use $ Searcher.hints . to length-}
        {-Searcher.selected      .= if selectInput then 0 else min 1 hintsLen-}
        {-Searcher.rollbackReady .= False-}

localClearSearcherHints :: Command State ()
localClearSearcherHints = do
    modifySearcher $ do
        Searcher.selectedPosition .= def
        Searcher.results          .= mempty
    updateDocumentation


search :: Input.Divided -> NodeHint.Database -> Maybe () -> [Result Hint.Hint]
search input nsData mayClassName = do
    let query          = input ^. Input.query
        metric         = def :: DefaultMetric
        {-strippedPrefix = Text.strip $ input ^. Input.prefix-}
        {-notNullInput   = not . Text.null $ convert input-}
        {-weights        = Just $ getWeights input mayClassName-}
        {-searchResult   = if notNullInput || isJust mayClassName-}
            {-then NodeSearcher.search                       query nsData weights-}
            {-else NodeSearcher.notConnectedEmptyInputSearch query nsData weights-}
        res = Search.search query (nsData ^. NodeHint.database) (const 1) metric
    (Result.hint %~ Hint.Node) <$> res
    {-if strippedPrefix == "def" then mempty-}
        {-else if query == "_"   then Searcher.wildcardMatch : searchResult-}
        {-else searchResult-}

{-getWeights :: Input.Divided -> Maybe ClassName  -> TypePreference-}
{-getWeights input mayClassName = do-}
    {-let query = input ^. Input.query-}
        {-strippedPrefix = Text.strip $ input ^. Input.prefix-}
        {-isFirstQuery = Text.null strippedPrefix-}
        {-searchForMethodsOnly = not (Text.null strippedPrefix)-}
            {-&& Text.last strippedPrefix == '.'-}
    {-Searcher.getWeights isFirstQuery searchForMethodsOnly mayClassName query-}





