{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Basic.UpdateSearcherHints where

import           Common.Action.Command                (Command)
import           Common.Prelude
import qualified Data.Aeson                           as Aeson
import qualified Data.ByteString.Lazy.Char8           as BS
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import           JS.Visualizers                       (sendVisualizationData)
import           LunaStudio.Data.NodeSearcher         (EntryType (Function), ImportName, ImportsHints, Match (Match),
                                                       ModuleHints (ModuleHints), RawEntry (RawEntry), TypePreferation (TypePreferation),
                                                       currentImports, imports, missingImports)
import qualified LunaStudio.Data.NodeSearcher         as NS
import           LunaStudio.Data.TypeRep              (ConstructorRep (ConstructorRep))
import           NodeEditor.Action.Batch              (searchNodes)
import           NodeEditor.Action.State.NodeEditor   (getLocalFunctions, getNodeSearcherData, getSearcher, inTopLevelBreadcrumb,
                                                       modifySearcher)
import           NodeEditor.React.Model.Searcher      (NodeModeInfo, Searcher, allCommands, className, updateCommandsResult,
                                                       updateNodeResult, waitingForTc)
import qualified NodeEditor.React.Model.Searcher      as Searcher
import           NodeEditor.React.Model.Visualization (visualizationId)
import           NodeEditor.State.Global              (State, nodeSearcherData)
import qualified NodeEditor.State.Global              as Global


type IsFirstQuery         = Bool
type SearchForMethodsOnly = Bool

selectNextHint :: Searcher -> Command State ()
selectNextHint _ = modifySearcher $ use (Searcher.hints . to length) >>= \hintsLen ->
    Searcher.selected %= min hintsLen . succ

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

setCurrentImports :: [ImportName] -> Command State ()
setCurrentImports importNames = do
    nodeSearcherData . currentImports .= importNames
    imps' <- (^. missingImports) <$> use nodeSearcherData
    when (not $ null imps') $ do
        Global.waitingForTc .= True
        modifySearcher $ waitingForTc .= True
        searchNodes imps'

updateDocs :: Command State ()
updateDocs = withJustM getSearcher $ \s -> withJust (s ^. Searcher.docVis) $ \docVis -> do
    let doc = maybe def (view NS.doc) $ s ^. Searcher.selectedMatch
    unless (Text.null doc) . liftIO $ do
        sendVisualizationData (docVis ^. visualizationId) (ConstructorRep "Text" def) (Text.pack . BS.unpack $ Aeson.encode doc)

localUpdateSearcherHintsPreservingSelection :: Command State ()
localUpdateSearcherHintsPreservingSelection = do
    maySelected <- maybe def (view Searcher.selectedMatch) <$> getSearcher
    localUpdateSearcherHints'
    withJust maySelected $ \selected -> do
        let equals e1 e2 = (e1 ^. NS.name == e2 ^. NS.name) && (e1 ^. NS.entryType == e2 ^. NS.entryType)
        entries <- maybe def (view Searcher.hints) <$> getSearcher
        withJust (findIndex (equals selected) entries) $ selectHint . (+1)
    updateDocs

localUpdateSearcherHints :: Command State ()
localUpdateSearcherHints = localUpdateSearcherHints' >> updateDocs

localUpdateSearcherHints' :: Command State ()
localUpdateSearcherHints' = unlessM inTopLevelBreadcrumb $ do
    nsData'        <- use nodeSearcherData
    localFunctions <- getLocalFunctions
    let localFunctionsImportName = "Local"
        nsData :: NS.NodeSearcherData
        nsData = nsData' & imports %~ Map.insert localFunctionsImportName (ModuleHints ((,def) <$> localFunctions) def)
                         & currentImports %~ (localFunctionsImportName :)
    modifySearcher $ do
        mayQuery <- preuse $ Searcher.input . Searcher._Divided
        m        <- use Searcher.mode
        let selectInput      = maybe True (Text.null . view Searcher.query) mayQuery
            (mode, hintsLen) = case m of
                (Searcher.Node _ nmi _) -> do
                    let isFirstQuery         q = Text.null . Text.dropWhile (== ' ') $ q ^. Searcher.prefix
                        strippedPrefix       q = Text.dropWhileEnd (== ' ') $ q ^. Searcher.prefix
                        searchForMethodsOnly q = if Text.null $ strippedPrefix q then False else Text.last (strippedPrefix q) == '.'
                        result = case mayQuery of
                            Nothing -> []
                            Just q  -> do
                                let query'     = q ^. Searcher.query
                                    weights    = Just $ getWeights (isFirstQuery q) (searchForMethodsOnly q) nmi query'
                                    searchRes' = NS.search query' nsData weights
                                    searchRes  = if query' == "_" then (Match (RawEntry query' def Function 1000000 . Just $ NS.ImportInfo localFunctionsImportName True) True 1000000 [(0, 1)]) : searchRes' else searchRes'
                                if Text.strip (q ^. Searcher.prefix) == "def"
                                    then def
                                    else takeWhile (view NS.exactMatch) searchRes
                    (updateNodeResult result m, length result)
                Searcher.Command {} -> do
                    let result = maybe [] (\q -> NS.searchCommands (q ^. Searcher.query) allCommands) mayQuery
                    (updateCommandsResult result m, length result)
                _                   -> (m, 0)
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

getWeights :: IsFirstQuery -> SearchForMethodsOnly -> NodeModeInfo -> Text -> TypePreferation
getWeights _     True _   _ = TypePreferation 0 0 (def, def) 1 0
getWeights False _    _   q = TypePreferation 0.7 0.5 (def, def) 0.3 (if not (Text.null q) && isUpper (Text.head q) then 0.6 else 0.1)
getWeights _     _    nmi q = case nmi ^. className of
    Nothing -> TypePreferation 0.5 0.7 (def, def) 0.3 (if not (Text.null q) && isUpper (Text.head q) then 0.9 else 0.2)
    Just cn -> TypePreferation 0.2 0.3 (Set.singleton cn, 0.7) 0.5 0.1
