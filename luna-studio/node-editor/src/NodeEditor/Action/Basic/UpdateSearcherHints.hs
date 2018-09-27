{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Basic.UpdateSearcherHints where

import           Common.Action.Command                (Command)
import           Common.Prelude
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Lazy.Char8           as BS
import qualified Data.JSString                        as JSString
import qualified Data.Map                             as Map
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import qualified IdentityString                       as IS
import           JS.Visualizers                       (sendVisualizationData)
import           LunaStudio.Data.NodeSearcher         (EntryType (Function), ImportName, ImportsHints, Match (Match),
                                                       ModuleHints (ModuleHints), RawEntry (RawEntry), TypePreference (TypePreference),
                                                       currentImports, imports, missingImports)
import qualified LunaStudio.Data.NodeSearcher         as NS
import           LunaStudio.Data.TypeRep              (ConstructorRep (ConstructorRep))
import           NodeEditor.Action.Batch              (searchNodes)
import           NodeEditor.Action.State.NodeEditor   (getLocalFunctions, getSearcher, inTopLevelBreadcrumb, modifySearcher)
import           NodeEditor.React.Model.Searcher      (NodeModeInfo, Searcher, allCommands, className, updateCommandsResult,
                                                       updateNodeResult, waitingForTc)
import qualified NodeEditor.React.Model.Searcher      as Searcher
import           NodeEditor.React.Model.Visualization (visualizationId)
import           NodeEditor.State.Global              (State, nodeSearcherData)
import qualified NodeEditor.State.Global              as Global


type IsFirstQuery         = Bool
type SearchForMethodsOnly = Bool

selectNextHint :: Searcher -> Command State ()
selectNextHint _ = modifySearcher $ use (Searcher.hints . to length)
    >>= \hintsLen -> Searcher.selected %= min hintsLen . succ

selectPreviousHint :: Searcher -> Command State ()
selectPreviousHint _ = modifySearcher $ Searcher.selected %= max 0 . pred

selectHint :: Int -> Command State ()
selectHint i = when (i >= 0) . modifySearcher $ do
    hLen <- use $ Searcher.hints . to length
    when (i <= hLen) $ Searcher.selected .= i

localAddSearcherHints :: ImportsHints -> Command State ()
localAddSearcherHints ih = do
    nodeSearcherData . imports %= Map.union ih
    localUpdateSearcherHintsPreservingSelection
    Global.waitingForTc .= False
    modifySearcher $ waitingForTc .= False

setCurrentImports :: Set ImportName -> Command State ()
setCurrentImports importNames = do
    nodeSearcherData . currentImports .= importNames
    imps' <- (^. missingImports) <$> use nodeSearcherData
    unless (null imps') $ do
        Global.waitingForTc .= True
        modifySearcher $ waitingForTc .= True
        searchNodes imps'

updateDocs :: Command State ()
updateDocs = withJustM getSearcher $ \s -> withJust (s ^. Searcher.docVis)
    $ \docVis -> do
        let doc = maybe def (view NS.doc) $ s ^. Searcher.selectedMatch
        unless (Text.null doc) . liftIO $
            sendVisualizationData
                (docVis ^. visualizationId)
                (ConstructorRep "Text" def)
                =<< (IS.fromJSString . JSString.pack . BS.unpack
                    $ Aeson.encode doc)

localUpdateSearcherHintsPreservingSelection :: Command State ()
localUpdateSearcherHintsPreservingSelection = do
    maySelected <- maybe def (view Searcher.selectedMatch) <$> getSearcher
    localUpdateSearcherHints'
    withJust maySelected $ \selected -> do
        let equals e1 e2
                =  (e1 ^. NS.name == e2 ^. NS.name)
                && (e1 ^. NS.entryType == e2 ^. NS.entryType)
        entries <- maybe def (view Searcher.hints) <$> getSearcher
        withJust (findIndex (equals selected) entries) $ selectHint . (+1)
    updateDocs

localUpdateSearcherHints :: Command State ()
localUpdateSearcherHints = localUpdateSearcherHints' >> updateDocs

localUpdateSearcherHints' :: Command State ()
localUpdateSearcherHints' = unlessM inTopLevelBreadcrumb $ do
    nsData'        <- use nodeSearcherData
    localFunctions <- getLocalFunctions
    let nsData :: NS.NodeSearcherData
        nsData = nsData'
            & imports %~ Map.insert
                localFunctionsImportName
                (ModuleHints ((,def) <$> localFunctions) def)
            & currentImports %~ Set.insert localFunctionsImportName
    modifySearcher $ do
        mayQuery <- preuse $ Searcher.input . Searcher._Divided
        m        <- use Searcher.mode
        let (mode, hintsLen) = case m of
                Searcher.Node _ nmi _ -> do
                    let hints input = search input nsData nmi
                        result      = maybe mempty hints mayQuery
                    (updateNodeResult result m, length result)
                Searcher.Command {} -> do
                    let hints input = NS.searchCommands
                            (input ^. Searcher.query)
                            allCommands
                        result = maybe mempty hints mayQuery
                    (updateCommandsResult result m, length result)
                _                   -> (m, 0)
            selectInput = maybe True (Text.null . view Searcher.query) mayQuery

        Searcher.selected      .= if selectInput then 0 else min 1 hintsLen
        Searcher.rollbackReady .= False
        Searcher.mode          .= mode

localClearSearcherHints :: Command State ()
localClearSearcherHints = do
    modifySearcher $ do
        Searcher.selected      .= def
        Searcher.rollbackReady .= False
        Searcher.mode          %= \case
            Searcher.Command         _ -> Searcher.Command def
            Searcher.Node     nl nmi _ -> Searcher.Node nl nmi def
            Searcher.NodeName nl     _ -> Searcher.NodeName nl def
            Searcher.PortName pr     _ -> Searcher.PortName pr def
    updateDocs

localFunctionsImportName :: Text
localFunctionsImportName = "Local"

wildcardMatch :: Match
wildcardMatch
    = Match wildcardEntry NS.CaseSensitiveEquality 1000000 [(0,1)] where
        wildcardImportInfo = NS.ImportInfo localFunctionsImportName True
        wildcardEntry = RawEntry
            "_"
            def
            Function
            1000000
            (Just wildcardImportInfo)

search :: Searcher.DividedInput -> NS.NodeSearcherData -> NodeModeInfo
    -> [Match]
search input nsData nmi = let
        query          = input ^. Searcher.query
        strippedPrefix = Text.strip $ input ^. Searcher.prefix
        searchResult   = NS.search query nsData (Just $ getWeights input nmi)
        hints          = takeWhile
            (\m -> m ^. NS.matchType /= NS.NotFullyMatched)
            searchResult
    in if strippedPrefix == "def" then mempty
        else if query == "_" then wildcardMatch : hints
        else hints

getWeights :: Searcher.DividedInput -> NodeModeInfo -> TypePreference
getWeights input nmi = getWeights'
    isFirstQuery
    searchForMethodsOnly
    nmi
    query where
        query = input ^. Searcher.query
        strippedPrefix = Text.strip $ input ^. Searcher.prefix
        isFirstQuery = Text.null strippedPrefix
        searchForMethodsOnly = not (Text.null strippedPrefix)
            && Text.last strippedPrefix == '.'

getWeights' :: IsFirstQuery -> SearchForMethodsOnly -> NodeModeInfo -> Text
    -> TypePreference
getWeights' _     True _   _ = TypePreference 0 0 (def, def) 1 0
getWeights' False _    _   q = TypePreference 0.7 0.5 (def, def) 0.3
    $ if not (Text.null q) && isUpper (Text.head q) then 0.6 else 0.1
getWeights' _     _    nmi q = case nmi ^. className of
    Nothing -> TypePreference 0.5 0.7 (def, def) 0.3
        $ if not (Text.null q) && isUpper (Text.head q) then 0.9 else 0.2
    Just cn -> TypePreference 0.2 0.3 (Set.singleton cn, 0.7) 0.5 0.1


