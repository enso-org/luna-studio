module NodeEditor.Action.Basic.UpdateSearcherHints where

import Common.Prelude

import qualified Data.Aeson                                   as Aeson
import qualified Data.Array                                   as Array
import qualified Data.ByteString.Lazy.Char8                   as BS
import qualified Data.JSString                                as JSString
import qualified Data.Map                                     as Map
import qualified Data.Set                                     as Set
import qualified Data.Text                                    as Text
import qualified IdentityString                               as IS
import qualified JS.SearcherEngine                            as SearcherEngine
import qualified LunaStudio.Data.Searcher.Hint                as Hint
import qualified LunaStudio.Data.Searcher.Hint.Library        as Library
import qualified LunaStudio.Data.Searcher.Hint.Class          as Class
{-import qualified LunaStudio.Data.Searcher.Hint.Node           as NodeHint-}
import qualified NodeEditor.React.Model.Searcher              as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint.Node    as NodeHint
import qualified NodeEditor.React.Model.Searcher.Hint         as Hint
import qualified NodeEditor.React.Model.Searcher.Input        as Input
{-import qualified NodeEditor.React.Model.Searcher.Mode.Command as CommandSearcher-}
import qualified NodeEditor.React.Model.Searcher.Mode       as Mode
import qualified NodeEditor.React.Model.Searcher.Mode.Node  as NodeMode
import qualified NodeEditor.React.Model.Searcher.Mode.Node    as NodeSearcher
import qualified NodeEditor.React.Model.Visualization         as Visualization
import qualified NodeEditor.State.Global                      as Global
import qualified Searcher.Engine.Data.Match                   as Match
import qualified Searcher.Engine.Data.Database                as DB
{-import qualified Searcher.Engine.Data.Result                  as Result-}
{-import qualified Searcher.Engine.Search                       as Search-}

import Common.Action.Command              (Command)
import Common.Debug                       (timeAction)
import Control.DeepSeq                    (force)
import Control.Exception.Base             (evaluate)
import Data.Map                           (Map)
import Data.Ord                           (comparing)
import Data.Set                           (Set)
import Data.Text                          (Text)
import JS.Visualizers                     (sendVisualizationData)
import LunaStudio.Data.PortRef              (OutPortRef)
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
{-import Searcher.Engine.Data.Result        (Result)-}
import JS.SearcherEngine (Result)
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
    Searcher.selectedPosition %= fmap (min hintsLen) . positionSucc

selectPreviousHint :: Command State ()
selectPreviousHint = modifySearcher $ Searcher.selectedPosition %= positionPred

selectHint :: Maybe Int -> Command State ()
selectHint i = modifySearcher $ do
    hLen <- fmap Just $ use $ Searcher.results . to length
    when (i <= hLen) $ Searcher.selectedPosition .= i

localAddSearcherHints :: SearcherLibraries -> Command State ()
localAddSearcherHints libHints = do
    oldDb <- use Global.searcherDatabase
    newDb <- NodeHint.insertSearcherLibraries libHints oldDb
    Global.searcherDatabase .= newDb
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
localUpdateSearcherHintsPreservingSelection = do
    maySelected <- maybe def (view Searcher.selectedResult) <$> getSearcher
    localUpdateSearcherHints'
    withJust maySelected $ \selected -> do
        let equals h1 h2 = h1 ^. DB.text == h2 ^. DB.text
        hints <- maybe def (view Searcher.results) <$> getSearcher
        withJust (findIndex (equals selected) hints) $ selectHint . Just
    updateDocumentation

localUpdateSearcherHints :: Command State ()
localUpdateSearcherHints = localUpdateSearcherHints' >> updateDocumentation

localUpdateSearcherHints' :: Command State ()
localUpdateSearcherHints' = timeAction "localUpdateSearcherHints'" $ unlessM inTopLevelBreadcrumb $ do
    withJustM getSearcher $ \searcher -> do
        let mayQuery = searcher ^? Searcher.input . Input._DividedInput
            query    = fromMaybe def mayQuery
        newHints <- case Mode.isExpressionSearcher $ searcher ^. Searcher.mode of
            True -> do
                nsData           <- use Global.searcherDatabase
                localFunctions   <- getLocalFunctions
                localFunctionsDb <- NodeHint.mkLocalFunctionsDb localFunctions
                let mayClassName = searcher ^? Searcher.mode
                        . Mode._Node . NodeMode.mode . NodeMode._ExpressionMode
                        . NodeMode.parent . _Just
                timeAction "search" $ search query localFunctionsDb nsData mayClassName
            False -> pure mempty
        modifySearcher $ do
            Searcher.results .= newHints
            let selectInput = maybe True (Text.null . view Input.query) mayQuery
            hintsLen <- use $ Searcher.results . to length
            Searcher.selectedPosition .= if selectInput || hintsLen == 0 then Nothing else Just 0

localClearSearcherHints :: Command State ()
localClearSearcherHints = do
    modifySearcher $ do
        Searcher.selectedPosition .= def
        Searcher.results          .= mempty
    updateDocumentation

getConnectedPortRef :: Command State (Maybe OutPortRef)
getConnectedPortRef = do
    s <- getSearcher
    pure $ s ^? _Just . Searcher.mode . Mode._Node . NodeMode.mode
              . NodeMode._ExpressionMode . NodeMode.newNode . _Just
              . NodeMode.connectionSource . _Just

updateClassName :: Maybe Class.Name -> Command State ()
updateClassName cl = do
    modifySearcher $ do
        Searcher.mode . Mode._Node . NodeMode.mode . NodeMode._ExpressionMode . NodeMode.parent .= cl
    localUpdateSearcherHintsPreservingSelection

scoreTextMatch :: Text -> NodeHint.Database -> Command State [Result NodeHint.Node]
scoreTextMatch query nsData = do
    let nodesMapping = nsData ^. NodeHint.nodes
    case Text.null query of
        True ->
            let mkResult r = SearcherEngine.Result r 0 $ SearcherEngine.Match []
            in pure $ mkResult <$> Array.elems nodesMapping
        False -> do
            let db = nsData ^. NodeHint.database
            res <- SearcherEngine.query db query
            pure $ (nodesMapping Array.!) <<$>> res

scoreClassMembership :: Maybe Class.Name -> [Result NodeHint.Node] -> [Result NodeHint.Node]
scoreClassMembership Nothing = id
scoreClassMembership (Just clName) = fmap adjustMethodScore where
    adjustMethodScore result
        = result & SearcherEngine.score
            +~ classScore (result ^. SearcherEngine.hint)
    classScore node = if node ^. NodeHint.kind == NodeHint.Method clName
                      then 1
                      else 0

scoreLocalFuns :: Maybe Class.Name -> [Result NodeHint.Node] -> [Result NodeHint.Node]
scoreLocalFuns Nothing   = fmap (SearcherEngine.score +~ 1)
scoreLocalFuns (Just cl) = id

fullDbSearch :: Input.Divided -> NodeHint.Database -> NodeHint.Database -> Maybe Class.Name -> Command State [Result Hint.Hint]
fullDbSearch input localDb nsData mayClassName = do
    let query = input ^. Input.query
    scoredText  <- scoreTextMatch query nsData
    scoredLocal <- scoreTextMatch query localDb
    let classBonus    = scoreClassMembership mayClassName scoredText
        localFunBonus = scoreLocalFuns mayClassName scoredLocal
        allHints      = localFunBonus <> classBonus
        sorted        = sortBy (comparing $ negate . view SearcherEngine.score) allHints
    pure $ Hint.Node <<$>> sorted

search :: Input.Divided -> NodeHint.Database -> NodeHint.Database -> Maybe Class.Name -> Command State [Result Hint.Hint]
search input localDb nsData mayClassName =
    if Text.strip (input ^. Input.prefix) == "def"
        then pure mempty
        else fullDbSearch input localDb nsData mayClassName
