-- INFO FOR @pmlodawski from dead soldier:
-- This file should replace UpdateSearcherHints.hs and contain all basic logic
-- for searcher. It is started but not finished - I guess you can tell my intentions
-- from what I've coded here. This should basically be reimplementation of
-- UpdateSearcherHints.hs module and parts of Action/Searcher.hs
-- In previous design there were some special wetghts that where given to 
-- methods/constructors, functions and class members - it was encoded in
-- TypePreference datatype. Currently this concept is replaced with first argument
-- of getScore function from fuzzy-text lib, module Result.hs. Basically now
-- you provide weight by passing a function that returns weight of hint based on
-- hint you are scoring now. The logic there should stay the same - it just
-- needs adjustment to new API.

module NodeEditor.Action.Basic.Searcher where

import Common.Prelude

import qualified Data.Aeson                                   as Aeson
import qualified Data.ByteString.Lazy.Char8                   as BS
import qualified Data.JSString                                as JSString
import qualified Data.Set                                     as Set
import qualified Data.Text                                    as Text
import qualified IdentityString                               as IS
import qualified JS.Searcher                                  as SearcherJS
import qualified JS.Visualizers                               as VisualizersJS
import qualified Luna.Syntax.Text.Lexer                       as Lexer
import qualified LunaStudio.Data.ScreenPosition               as ScreenPosition
import qualified LunaStudio.Data.Searcher.Hint                as Hint
import qualified LunaStudio.Data.Searcher.Hint.Class          as Class
import qualified LunaStudio.Data.Searcher.Hint.Library        as Library
import qualified LunaStudio.Data.Size                         as Size
import qualified LunaStudio.Data.TypeRep                      as TypeRep
import qualified NodeEditor.Action.Basic.ModifyCamera         as Camera
import qualified NodeEditor.Action.Batch                      as Batch
import qualified NodeEditor.Action.State.App                  as App
import qualified NodeEditor.Action.State.NodeEditor           as NodeEditor
import qualified NodeEditor.Action.State.Scene                as Scene
import qualified NodeEditor.React.Model.Node                  as Node
import qualified NodeEditor.React.Model.Node.ExpressionNode   as ExpressionNode
import qualified NodeEditor.React.Model.Node.ExpressionNode   as Node
import qualified NodeEditor.React.Model.NodeEditor            as NodeEditor hiding
                                                                             (getExpressionNode)
import qualified NodeEditor.React.Model.Port                  as Port
import qualified NodeEditor.React.Model.Searcher              as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint         as Hint
import qualified NodeEditor.React.Model.Searcher.Hint.Command as CommandHint
import qualified NodeEditor.React.Model.Searcher.Hint.Node    as NodeHint
import qualified NodeEditor.React.Model.Searcher.Input        as Input
import qualified NodeEditor.React.Model.Searcher.Mode         as Mode
import qualified NodeEditor.React.Model.Searcher.Mode.Node    as NodeSearcher
import qualified NodeEditor.React.Model.Visualization         as Visualization
import qualified NodeEditor.State.Global                      as Global
import qualified Searcher.Engine.Data.Database                as Database
import qualified Searcher.Engine.Data.Result                  as Result

import Common.Action.Command                 (Command)
import Data.Set                              (Set)
import LunaStudio.Data.Matrix                (invertedTranslationMatrix,
                                              translationMatrix)
import LunaStudio.Data.NodeLoc               (NodeLoc)
import LunaStudio.Data.Searcher.Hint.Library (SearcherLibraries)
import LunaStudio.Data.TypeRep               (ConstructorRep (ConstructorRep))
import LunaStudio.Data.Vector2               (Vector2 (Vector2))
import NodeEditor.React.Model.Constants      (searcherHeight, searcherWidth)
import NodeEditor.React.Model.Node           (ExpressionNode)
import NodeEditor.React.Model.Searcher.Hint  (Hint)
import NodeEditor.React.Model.Searcher.Mode  (Mode)
import NodeEditor.React.Model.Visualization  (VisualizationMode)
import NodeEditor.State.Global               (State)
import Searcher.Engine.Data.Result           (Result)


isSearcherNode :: NodeLoc -> Command State Bool
isSearcherNode nl = maybe False matchesNodeLoc <$> NodeEditor.getSearcher where
    matchesNodeLoc s = Just nl == getNodeLoc s
    getNodeLoc     s = s ^? Searcher.mode . Mode._Node . NodeSearcher.nodeLoc
{-# INLINE isSearcherNode #-}

setClassName :: Maybe Class.Name -> Command State ()
setClassName className = NodeEditor.modifySearcher
    $ Searcher.mode . Mode._Node . NodeSearcher.mode
    . NodeSearcher._ExpressionMode . NodeSearcher.parent .= className
{-# INLINE setClassName #-}

handleNodeUpdate :: ExpressionNode -> Command State ()
handleNodeUpdate updatedNode = let
    nl               = updatedNode ^. ExpressionNode.nodeLoc
    mayConnectedPort = listToMaybe $ ExpressionNode.outPortsList updatedNode
    toClassName p    = convert <$> p ^? Port.valueType . TypeRep._TCons . _1
    className        = maybe mempty toClassName mayConnectedPort
    in whenM (isSearcherNode nl) $ do
        setClassName className
        preserveSelection searchHints
        updateDocumentation
{-# INLINE handleNodeUpdate #-}

setImportedLibraries :: Set Library.Name -> Command State ()
setImportedLibraries libs = do
    Global.searcherDatabase . NodeHint.imported .= libs
    missingLibs <- use $ Global.searcherDatabase . NodeHint.missingLibraries
    unless (null missingLibs) $ Batch.searchNodes missingLibs
{-# INLINE setImportedLibraries #-}


updateVisualizationMode :: VisualizationMode -> Command State ()
updateVisualizationMode visMode = NodeEditor.modifySearcher
    $ Searcher.mode . Mode._Node . NodeSearcher.documentationVisualization
    . _Just . Visualization.visualizationMode .= visMode
{-# INLINE updateVisualizationMode #-}

updateDatabase :: SearcherLibraries -> Command State ()
updateDatabase libs = do
    Global.searcherDatabase %= NodeHint.insertSearcherLibraries libs
    preserveSelection searchHints
    updateDocumentation
{-# INLINE updateDatabase #-}

open :: Text -> Mode -> Command State ()
open input mode = do
    NodeEditor.modifyNodeEditor $ NodeEditor.searcher
        ?= Searcher.Searcher def False mempty def mode
    adjustCamera
    let inputLen = Text.length input
    modifyInput input inputLen inputLen
    App.renderIfNeeded
    SearcherJS.focus
{-# INLINE open #-}

modifyInput :: Text -> Int -> Int -> Command State ()
modifyInput input selectionStart selectionEnd = do
    updateInput input selectionStart selectionEnd
    replaceInput selectionStart selectionEnd
{-# INLINE modifyInput #-}

replaceInput :: Int -> Int -> Command State ()
replaceInput selectionStart selectionEnd = do
    NodeEditor.modifySearcher $ Searcher.replaceInput .= True
    App.renderIfNeeded
    SearcherJS.setSelection selectionStart selectionEnd
    NodeEditor.modifySearcher $ Searcher.replaceInput .= False
{-# INLINE replaceInput #-}


adjustCamera :: Command State ()
adjustCamera = withJustM NodeEditor.getSearcher $ \searcher -> do
    let mayNS            = searcher ^? Searcher.mode . Mode._Node
        isExpressionMode = has
            (_Just . NodeSearcher.mode . NodeSearcher._ExpressionMode) mayNS
        isNodeNameMode   = has
            (_Just . NodeSearcher.mode . NodeSearcher._NodeNameMode) mayNS
        maySearcherNl       = view NodeSearcher.nodeLoc <$> mayNS
        mayNewNodePosition  = mayNS ^? _Just . NodeSearcher.mode
            . NodeSearcher._ExpressionMode . NodeSearcher.newNode
            .  _Just . NodeSearcher.position
        getNodeTop nl       = fmap2 (view Node.topPosition)
            $ NodeEditor.getExpressionNode nl
        mayNodeTopPositionM = maybe (pure Nothing) getNodeTop maySearcherNl
        mayWorkspaceSearcherBottomM = maybe
            mayNodeTopPositionM
            (pure . Just . Node.toNodeTopPosition)
            mayNewNodePosition
        searcherRows = if isExpressionMode then 11
            else if isNodeNameMode then 2
            else 0
        bottomToTopYShift = -1 * searcherRows * searcherHeight
        bottomToTop       = ScreenPosition.move (Vector2 0 bottomToTopYShift)

    mayScreenSize     <- Scene.getScreenSize
    maySearcherBottom <- mapM Scene.translateToScreen
        =<< mayWorkspaceSearcherBottomM
    let maySearcherTop = bottomToTop <$> maySearcherBottom
        getCameraDelta searcherBottom searcherTop screenSize =
            let topX                = searcherTop ^. ScreenPosition.x
                topY                = searcherTop ^. ScreenPosition.y
                bottomY             = searcherBottom ^. ScreenPosition.y
                screenWidth         = screenSize ^. Size.width
                screenHeight        = screenSize ^. Size.height
                overRightEdge       = topX + searcherWidth / 2 > screenWidth
                overLeftEdge        = topX - searcherWidth / 2 < 0
                overTopEdge         = topY < 0
                xShift = if searcherWidth > screenWidth
                        then Nothing
                    else if overRightEdge
                        then Just $ topX + searcherWidth / 2 - screenWidth
                    else if overLeftEdge
                        then Just $ topX - searcherWidth / 2
                        else Nothing
                yShift = if bottomY - topY > screenHeight
                        then Just $ bottomY - screenHeight
                    else if overTopEdge
                        then Just topY
                        else Nothing
            in if isNothing xShift && isNothing yShift
                then Nothing
                else Just $ Vector2 (fromMaybe def xShift) (fromMaybe def yShift)
        mayCameraDelta = join $ getCameraDelta
            <$> maySearcherBottom
            <*> maySearcherTop
            <*> mayScreenSize

    withJust mayCameraDelta $ \delta -> Camera.modifyCamera
        (invertedTranslationMatrix delta)
        (translationMatrix delta)
{-# INLINE adjustCamera #-}

updateInput :: Text -> Int -> Int -> Command State ()
updateInput input selectionStart selectionEnd = do
    mayMode <- view Searcher.mode `fmap2` NodeEditor.getSearcher
    let inputStream = Lexer.evalDefLexer $ convert input
        searcherInput
            = if selectionStart /= selectionEnd then Input.RawInput input
            else if Text.null input             then Input.DividedInput def
            else Input.fromStream input inputStream selectionStart
        inputDivided = has Input._DividedInput searcherInput
        mayLambdaArgsAndEndPos = Input.findLambdaArgsAndEndOfLambdaArgs
            (convert input)
            inputStream
        lambdaArgs      = maybe mempty fst mayLambdaArgsAndEndPos
        mayLambdaEndPos = snd <$> mayLambdaArgsAndEndPos
        isNodeSearcher  = has (_Just . Mode._Node) mayMode
        searchNS        = do
            let withEndPos endPos = if selectionStart < endPos
                    then clearHints else searchHints
            setArgumentsNames lambdaArgs
            maybe searchHints withEndPos mayLambdaEndPos
    NodeEditor.modifySearcher $ Searcher.input .= searcherInput
    if not inputDivided    then clearHints
    else if isNodeSearcher then searchNS
    else searchHints
{-# INLINE updateInput #-}

setArgumentsNames :: [Text] -> Command State ()
setArgumentsNames args = NodeEditor.modifySearcher
    $ Searcher.mode . Mode._Node . NodeSearcher.mode
    . NodeSearcher._ExpressionMode . NodeSearcher.arguments .= args
{-# INLINE setArgumentsNames #-}

clearHints :: Command State ()
clearHints = do
    NodeEditor.modifySearcher $ do
        Searcher.selectedPosition .= def
        Searcher.results          .= mempty
    updateDocumentation
{-# INLINE clearHints #-}

updateDocumentation :: Command State ()
updateDocumentation = withJustM NodeEditor.getSearcher $ \searcher -> let
    mayDocVis   = searcher ^? Searcher.mode . Mode._Node
        . NodeSearcher.documentationVisualization . _Just
    fromTxt txt = if Text.null txt then Nothing else Just txt
    mayDoc      = maybe Nothing fromTxt $ searcher
        ^? Searcher.selectedResult . _Just . Hint.documentation
    withDoc f   = withJust ((,) <$> mayDocVis <*> mayDoc) $ uncurry f
    cRep        = ConstructorRep "Text" def
    toIS        = IS.fromJSString . JSString.pack . BS.unpack . Aeson.encode
    sendDocData = \docVis doc -> let
        visId = docVis ^. Visualization.visualizationId
        in liftIO $ VisualizersJS.sendVisualizationData visId cRep =<< toIS doc
    in withDoc sendDocData
{-# INLINE updateDocumentation #-}

getLocalFunctionsDatabase :: Command State (Database.Database NodeHint.Node)
getLocalFunctionsDatabase = do
    functionsNames <- toList . Set.fromList . fmap (view Port.name)
        . concatMap Node.outPortsList <$> NodeEditor.getAllNodes
    let getLambdaArgsNames s = fromMaybe mempty
            $ s ^? Searcher.mode . Mode._Node . NodeSearcher.mode
                . NodeSearcher._ExpressionMode . NodeSearcher.arguments
    lambdaArgsNames <- maybe mempty getLambdaArgsNames
        <$> NodeEditor.getSearcher
    let libInfo   = Library.Info NodeHint.localFunctionsLibraryName True
        hintsText = functionsNames <> lambdaArgsNames
        rawHints  = flip Hint.Raw mempty <$> hintsText
        hints     = flip NodeHint.fromFunction libInfo <$> rawHints
    pure $ Database.mk hints
{-# INLINE getLocalFunctionsDatabase #-}

getDatabases :: Command State [Database.Database Hint]
getDatabases = do
    maySearcher <- NodeEditor.getSearcher
    let isExpressionMode = has
            (_Just . Searcher.mode . Mode._Node . NodeSearcher.mode
            . NodeSearcher._ExpressionMode)
            maySearcher
        isCommandMode = has (_Just . Searcher.mode . Mode._Command) maySearcher
        nodeHintsM    = use $ Global.searcherDatabase . NodeHint.database
        localHintsM   = getLocalFunctionsDatabase
        exprHintsM    = do
            nodeHints  <- nodeHintsM
            localHints <- localHintsM
            let nodeHints'  = nodeHints  & Database.hints 
                    %~ fmap (fmap Hint.Node)
                localHints' = localHints & Database.hints 
                    %~ fmap (fmap Hint.Node)
            pure [nodeHints', localHints']
        commandHints = CommandHint.database 
            & Database.hints %~ fmap (fmap Hint.Command)
    if isExpressionMode   then exprHintsM
    else if isCommandMode then pure [commandHints]
    else pure mempty
{-# INLINE getDatabases #-}

searchHints :: Command State ()
searchHints = do
    databases <- getDatabases
    pure ()

updateHints :: [Result Hint] -> Command State ()
updateHints results = NodeEditor.modifySearcher $ do
    Searcher.results .= results
    

preserveSelection :: Command State () -> Command State ()
preserveSelection action = do
    maySearcher <- NodeEditor.getSearcher
    let maySelectedResult
            = maybe Nothing (view Searcher.selectedResult) maySearcher
        maySelectedHint = view Result.hint <$> maySelectedResult
    action
    NodeEditor.modifySearcher $ do
        results <- use Searcher.results
        let newSelected = if null results
                then Nothing
                else Just $ case maySelectedHint of
                    Nothing -> 0
                    Just h  -> fromMaybe 0
                        $! findIndex (\r -> r ^. Result.hint == h) results
        Searcher.selectedPosition .= newSelected
{-# INLINE preserveSelection #-}
