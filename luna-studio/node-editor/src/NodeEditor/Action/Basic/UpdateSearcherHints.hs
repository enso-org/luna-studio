module NodeEditor.Action.Basic.UpdateSearcherHints where

import Common.Prelude

import qualified Data.Aeson                                 as Aeson
import qualified Data.ByteString.Lazy.Char8                 as BS
import qualified Data.JSString                              as JSString
import qualified Data.Text                                  as Text
import qualified IdentityString                             as IS
import qualified LunaStudio.Data.Searcher.Hint.Library      as Library
import qualified LunaStudio.Data.Searcher.Hint.Class        as Class
import qualified NodeEditor.React.Model.Searcher            as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint       as Hint
import qualified NodeEditor.React.Model.Searcher.Hint.Node  as NodeHint
import qualified NodeEditor.React.Model.Searcher.Input      as Input
import qualified NodeEditor.React.Model.Searcher.Mode       as Mode
import qualified NodeEditor.React.Model.Searcher.Mode.Node  as NodeMode
import qualified NodeEditor.React.Model.Visualization       as Visualization
import qualified NodeEditor.State.Global                    as Global
import qualified Searcher.Data.Class                        as Searcher
import qualified Searcher.Data.Database                     as Database
import qualified Searcher.Data.Result                       as Result
import qualified Searcher.Engine                            as SearcherEngine

import Common.Action.Command              (Command)
import Control.DeepSeq                    (force)
import Data.Ord                           (comparing)
import Data.Set                           (Set)
import Data.Text                          (Text)
import JS.Visualizers                     (sendVisualizationData)
import LunaStudio.Data.PortRef            (OutPortRef)
import LunaStudio.Data.TypeRep            (ConstructorRep (ConstructorRep))
import NodeEditor.Action.Batch            (searchNodes)
import NodeEditor.Action.State.NodeEditor (getLocalFunctions, getSearcher,
                                           inTopLevelBreadcrumb, modifySearcher)
import NodeEditor.State.Global            (State)
import Searcher.Data.Result               (Result)


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

addDatabaseHints :: Library.Set -> Command State ()
addDatabaseHints libHints = do
    oldDb <- use Global.searcherDatabase
    let newDb = NodeHint.insertSearcherLibraries libHints oldDb
    Global.searcherDatabase .= newDb
    updateHintsPreservingSelection

setImportedLibraries :: Set Library.Name -> Command State ()
setImportedLibraries libs = do
    Global.searcherDatabase %= NodeHint.setImportedLibraries libs
    missingLibs <- use $ Global.searcherDatabase . NodeHint.missingLibraries
    unless (null missingLibs) $
        searchNodes missingLibs

updateDocumentation :: Command State ()
updateDocumentation = withJustM getSearcher $ \s -> do
    let mayDocVis = s ^? Searcher.documentationVisualization
        mayDoc = s ^? Searcher.selectedResult . _Just . Searcher.documentation
        mayDocData = (,) <$> mayDocVis <*> mayDoc
    withJust mayDocData $ \(docVis, doc) -> liftIO $ sendVisualizationData
        (docVis ^. Visualization.visualizationId)
        (ConstructorRep "Text" def)
        =<< (IS.fromJSString . JSString.pack . BS.unpack $ Aeson.encode doc)

updateHintsPreservingSelection :: Command State ()
updateHintsPreservingSelection = do
    maySelected <- maybe def (view Searcher.selectedResult) <$> getSearcher
    updateHints'
    withJust maySelected $ \selected -> do
        let equals = (==) `on` view Searcher.text
        hints <- maybe def (view Searcher.results) <$> getSearcher
        withJust (findIndex (equals selected) hints) $ selectHint . Just
    updateDocumentation

updateHints :: Command State ()
updateHints = updateHints' >> updateDocumentation

updateHints' :: Command State ()
updateHints' = unlessM inTopLevelBreadcrumb $ do
    nsData    <- use Global.searcherDatabase
    localFuns <- getLocalFunctions
    modifySearcher $ do
        mayQuery <- preuse $ Searcher.input . Input._DividedInput
        let query = fromMaybe def mayQuery
        isExprSearcher <- uses Searcher.mode Mode.isExpressionSearcher
        newHints <- case isExprSearcher of
            True -> do
                let localFunctionsDb = NodeHint.mkLocalFunctionsDb localFuns
                mayClassName <- preuse $ Searcher.mode
                    . Mode._Node . NodeMode.mode . NodeMode._ExpressionMode
                    . NodeMode.parent . _Just
                pure $ search query localFunctionsDb nsData mayClassName
            False -> pure mempty
        Searcher.results .= newHints
        Searcher.waiting .= (Database.size (nsData ^. NodeHint.database) == 0)
        let selectInput = maybe True (Text.null . view Input.query) mayQuery
        hintsLen <- use $ Searcher.results . to length
        Searcher.selectedPosition .= if selectInput || hintsLen == 0
                                         then Nothing
                                         else Just 0

clearHints :: Command State ()
clearHints = do
    modifySearcher $ do
        Searcher.selectedPosition .= def
        Searcher.results          .= mempty
    updateDocumentation

getConnectedPortRef :: Command State (Maybe OutPortRef)
getConnectedPortRef = let
    connectedPortLens = Searcher.mode . Mode._Node . NodeMode.connectedPortRef
    in join . fmap (preview connectedPortLens) <$> getSearcher

updateClassName :: Maybe Class.Name -> Command State ()
updateClassName cl = do
    modifySearcher $
        Searcher.mode . Mode._Node . NodeMode.mode
            . NodeMode._ExpressionMode . NodeMode.parent .= cl
    updateHintsPreservingSelection

scoreTextMatch :: Text -> NodeHint.Database -> [Result NodeHint.Node]
scoreTextMatch query nsData = case Text.null query of
    True ->
        let db = nsData ^. NodeHint.database
        in Result.make <$> Database.elems db
    False ->
        let db = nsData ^. NodeHint.database
        in SearcherEngine.query db query

bumpIf :: (a -> Bool) -> Double -> [Result a] -> [Result a]
bumpIf pred amt = fmap bump where
    bump result = if pred $ result ^. Result.hint
                      then result & Result.score +~ amt
                      else result

defaultBumpAmount :: Double
defaultBumpAmount = 1

snippetBumpAmount :: Double
snippetBumpAmount = 2

importedBumpAmount :: Double
importedBumpAmount = 0.1

bumpLocalFuns :: Input.SymbolKind -> [Result NodeHint.Node]
              -> [Result NodeHint.Node]
bumpLocalFuns Input.Argument = bumpIf (const True) defaultBumpAmount
bumpLocalFuns _ = id

bumpMethodsOf :: Class.Name -> [Result NodeHint.Node] -> [Result NodeHint.Node]
bumpMethodsOf cl = bumpIf (\res -> res ^. NodeHint.kind == NodeHint.Method cl)
                          defaultBumpAmount

bumpGlobalFuns :: [Result NodeHint.Node] -> [Result NodeHint.Node]
bumpGlobalFuns = bumpIf (not . has (NodeHint.kind . NodeHint._Method))
                        defaultBumpAmount

bumpAllMethods :: [Result NodeHint.Node] -> [Result NodeHint.Node]
bumpAllMethods = bumpIf (has $ NodeHint.kind . NodeHint._Method)
                        defaultBumpAmount

bumpOperators :: [Result NodeHint.Node] -> [Result NodeHint.Node]
bumpOperators = id

bumpGlobalSyms :: Input.SymbolKind -> Maybe Class.Name -> [Result NodeHint.Node]
               -> [Result NodeHint.Node]
bumpGlobalSyms Input.ExpressionStart (Just cl) = bumpMethodsOf cl
bumpGlobalSyms Input.ExpressionStart Nothing   = bumpGlobalFuns
bumpGlobalSyms Input.Function        _         = bumpGlobalFuns
bumpGlobalSyms Input.Argument        _         = id
bumpGlobalSyms Input.Method          _         = bumpAllMethods
bumpGlobalSyms Input.Operator        _         = bumpOperators

bumpSnippets :: [Result NodeHint.Node] -> [Result NodeHint.Node]
bumpSnippets = bumpIf (has $ NodeHint.kind . NodeHint._Snippet)
                      snippetBumpAmount

bumpImported :: [Result NodeHint.Node] -> [Result NodeHint.Node]
bumpImported = bumpIf (\hint -> hint ^. NodeHint.library . Library.imported)
                      importedBumpAmount

filterSnippets :: Input.Divided -> Maybe Class.Name -> [Result NodeHint.Node]
               -> [Result NodeHint.Node]
filterSnippets query className = let
    isNotSnippet = hasn't $ Result.hint . NodeHint.kind . NodeHint._Snippet
    isSnippetForClass k =
        k ^. Result.hint . NodeHint.kind == NodeHint.Snippet className
    in if Input.null query
        then filter ((||) <$> isNotSnippet <*> isSnippetForClass)
        else filter isNotSnippet

fullDbSearch :: Input.Divided -> NodeHint.Database -> NodeHint.Database
             -> Maybe Class.Name -> [Result Hint.Hint]
fullDbSearch input localDb nsData mayClassName = let
    query            = input ^. Input.query
    nextSym          = input ^. Input.nextSymbolPrediction
    scoredGlobal     = scoreTextMatch query nsData
    scoredLocal      = scoreTextMatch query localDb
    semanticLocal    = bumpLocalFuns  nextSym scoredLocal
    semanticGlobal   = bumpGlobalSyms nextSym mayClassName scoredGlobal
    filteredSnippets = filterSnippets input mayClassName semanticGlobal
    scoredSnippets   = bumpSnippets filteredSnippets
    scoredImports    = bumpImported scoredSnippets
    allHints         = semanticLocal <> scoredImports
    sorted           = sortBy (comparing $ negate . view Result.score) allHints
    in Hint.Node <<$>> sorted

search :: Input.Divided -> NodeHint.Database -> NodeHint.Database
       -> Maybe Class.Name -> [Result Hint.Hint]
search input localDb nsData mayClassName =
    if Text.strip (input ^. Input.prefix) == "def"
        then mempty
        else fullDbSearch input localDb nsData mayClassName

