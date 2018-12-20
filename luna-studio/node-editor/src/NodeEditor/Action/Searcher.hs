-- INFO FOR @pmlodawski from dead soldier:
-- This module should be trimmed from any low level logic of searcher. It should
-- be basically a wrapper in action concept (so implement class Action and use
-- its like other files do) just as it was before.

module NodeEditor.Action.Searcher where

import Common.Prelude

import qualified Data.Text                                  as Text
import qualified JS.Searcher                                as Searcher
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import qualified LunaStudio.Data.PortRef                    as PortRef
import qualified LunaStudio.Data.TypeRep                    as TypeRep
import qualified NodeEditor.Action.Basic                    as Basic
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import qualified NodeEditor.React.Model.Port                as Port
import qualified NodeEditor.React.Model.Searcher            as Searcher
import qualified NodeEditor.React.Model.Searcher.Input      as Input
import qualified NodeEditor.React.Model.Visualization       as Visualization
import qualified NodeEditor.React.View.App                  as App
import qualified NodeEditor.State.Global                    as Global
import qualified NodeEditor.State.UI                        as UI
import qualified Searcher.Engine.Data.Match                 as Match
import qualified Searcher.Engine.Data.Symbol                as Symbol
import qualified Searcher.Engine.Data.Symbol.Library        as Library

import Common.Action.Command                (Command)
import Common.Report                        (warning)
import Control.Arrow                        ((&&&))
import JS.Visualizers                       (registerVisualizerFrame)
import Luna.Syntax.Text.Lexer               (evalDefLexer)
import LunaStudio.Data.Geometry             (snap)
import LunaStudio.Data.Matrix               (invertedTranslationMatrix,
                                             translationMatrix)
import LunaStudio.Data.NodeLoc              (NodeLoc, NodePath)
import LunaStudio.Data.Port                 (AnyPortId (OutPortId'))
import LunaStudio.Data.PortRef              (AnyPortRef,
                                             OutPortRef (OutPortRef),
                                             toAnyPortRef)
import LunaStudio.Data.Position             (Position)
import LunaStudio.Data.ScreenPosition       (move, x, y)
import LunaStudio.Data.Size                 (height, width)
import LunaStudio.Data.TypeRep              (TypeRep (TCons))
import LunaStudio.Data.Vector2              (Vector2 (Vector2))
import NodeEditor.Action.Basic              (createNode,
                                             localClearSearcherHints,
                                             localUpdateSearcherHints,
                                             modifyCamera, renameNode,
                                             renamePort, setNodeExpression,
                                             updateDocumentation)
import NodeEditor.Action.Batch              (addImport)
import NodeEditor.Action.State.Action       (beginActionWithKey,
                                             continueActionWithKey,
                                             removeActionFromState,
                                             updateActionWithKey)
import NodeEditor.Action.State.App          (renderIfNeeded)
import NodeEditor.Action.State.NodeEditor   (findSuccessorPosition,
                                             getExpressionNode, getPort,
                                             getSearcher, getSelectedNodes,
                                             modifyNodeEditor, modifySearcher)
import NodeEditor.Action.State.Scene        (getScreenSize, translateToScreen,
                                             translateToWorkspace)
import NodeEditor.Action.UUID               (getUUID)
import NodeEditor.Event.Event               (Event (Shortcut))
import NodeEditor.React.Model.Constants     (searcherHeight, searcherWidth)
import NodeEditor.React.Model.Searcher      (Match, Symbol)
import NodeEditor.React.Model.Visualization (RunningVisualization (RunningVisualization),
                                             VisualizerProperties (VisualizerProperties),
                                             getMdVisualizer)
import NodeEditor.State.Action              (Action (begin, continue, end, update),
                                             Searcher (Searcher),
                                             searcherAction)
import NodeEditor.State.Global              (State, visualizers)
import Text.Read                            (readMaybe)

import LunaStudio.Data.ScreenPosition (ScreenPosition)
import LunaStudio.Data.Size           (Size)

instance Action (Command State) Searcher where
    begin    = beginActionWithKey    searcherAction
    continue = continueActionWithKey searcherAction
    update   = updateActionWithKey   searcherAction
    end      = close

unavailableDocumentationMsg :: String
unavailableDocumentationMsg
    = "Documentation unavailable. Cannot find markdown visualizer."

mkDocumentationVisualization :: Command State (Maybe RunningVisualization)
mkDocumentationVisualization = getUUID >>= \uuid -> do
    mayVis <- use visualizers >>= getMdVisualizer
    let mayVisId :: Maybe (Visualization.VisualizerId)
        mayVisId   = view Visualization.visualizerId <$> mayVis
        mayVisProp :: Maybe Visualization.VisualizerProperties
        mayVisProp = flip VisualizerProperties mayVisId <$> mayVis
    when (isNothing mayVisProp) $ warning unavailableDocumentationMsg
    forM mayVisProp $ \vp -> getUUID >>= \uuid -> do
        liftIO $ registerVisualizerFrame uuid
        pure $ RunningVisualization uuid def vp

emptyInputError :: Searcher.Mode -> Text
emptyInputError m = fieldName <> errSuffix where
    errSuffix = " cannot be empty."
    fieldName = case m of
        Searcher.CommandSearcher {} -> "Command"
        Searcher.NodeSearcher    ns -> case ns ^. Searcher.modeData of
            Searcher.ExpressionMode {} -> "Node expression"
            Searcher.NodeNameMode   {} -> "Node name"
            Searcher.PortNameMode   {} -> "Port name"

clearSearcherError :: Command State ()
clearSearcherError = modifySearcher $ Searcher.searcherError .= def

editSelectedNodeExpression :: Command State ()
editSelectedNodeExpression = getSelectedNodes >>= \case
    [n] -> editExpression $ n ^. Node.nodeLoc
    _   -> pure ()

editExpression :: NodeLoc -> Command State ()
editExpression nl =
    let getClassName n = convert <$> n ^? Node.inPortAt [Port.Self]
            . Port.valueType . TypeRep._TCons . _1
        mkExpressionData n = Searcher.ExpressionMode $ Searcher.ExpressionData
            Nothing
            (getClassName n)
            mempty
        mkExpressionNodesDataM n = Searcher.NodesData
            nl
            def
            (mkExpressionData n)
            <$> mkDocumentationVisualization
        mkExpressionSearcherModeM n
            = Searcher.NodeSearcher <$> mkExpressionNodesDataM n
    in withJustM (getExpressionNode nl) $ \n -> openWith
        (n ^. Node.code)
        =<< mkExpressionSearcherModeM n

editName :: NodeLoc -> Command State ()
editName nl = withJustM (getExpressionNode nl) $ \n -> openWith
    (maybe mempty id $ n ^. Node.name)
    $ Searcher.NodeSearcher $ Searcher.NodesData nl def Searcher.NodeNameMode def

editPortName :: OutPortRef -> Command State ()
editPortName portRef = withJustM (getPort portRef) $ \p -> openWith
    (p ^. Port.name)
    $ Searcher.NodeSearcher $ Searcher.NodesData
        (portRef ^. PortRef.nodeLoc)
        def
        (Searcher.PortNameMode
            $ Searcher.PortNameData $ portRef ^. PortRef.srcPortId)
        def

open :: Maybe Position -> Command State ()
open mayPosition = do
    let getConnectedNode selectedNodes = if length selectedNodes == 1
            then listToMaybe selectedNodes
            else Nothing
        mayConnectedNodeM = getConnectedNode <$> getSelectedNodes
        mayConnectedPortM = fmap join $ (listToMaybe . Node.outPortsList)
            `fmap2` mayConnectedNodeM
        mayConnectedNodeLocM = view Node.nodeLoc `fmap2` mayConnectedNodeM
        mayConnectedPortIdM  = view Port.portId  `fmap2` mayConnectedPortM
        mayConnectedPortRefM = do
            mayConnectedNodeLoc <- mayConnectedNodeLocM
            mayConnectedPortId <- mayConnectedPortIdM
            pure $ OutPortRef <$> mayConnectedNodeLoc <*> mayConnectedPortId
        notConnectedSearcherPositionM = maybe
            (translateToWorkspace =<< use (Global.ui . UI.mousePos))
            pure
            mayPosition
        positionM = fmap snap $ maybe
            notConnectedSearcherPositionM
            findSuccessorPosition
            =<< mayConnectedNodeM
        newNodeDataM = Searcher.NewNodeData
            <$> positionM
            <*> mayConnectedPortRefM
        getPortClassName p = convert
            <$> p ^? Port.valueType . TypeRep._TCons . _1
        mayClassNameM = maybe mempty getPortClassName <$> mayConnectedPortM
        expressionModeM = Searcher.ExpressionMode .:. Searcher.ExpressionData
            <$> (Just <$> newNodeDataM)
            <*> mayClassNameM
            <*> pure mempty
        mkNodesDataM nl = Searcher.NodesData
            nl
            mempty
            <$> expressionModeM
            <*> mkDocumentationVisualization
        mkNodeSearcherM nl = Searcher.NodeSearcher <$> mkNodesDataM nl

    nl <- convert . ((def :: NodePath), ) <$> getUUID
    openWith mempty =<< mkNodeSearcherM nl

adjustCameraToSearcher :: Searcher.Mode -> Command State ()
adjustCameraToSearcher mode = do
    let mayNodesData      = mode         ^? Searcher._NodeSearcher
        mayModeData       = mayNodesData ^? _Just . Searcher.modeData
        maySearcherNl     = mayNodesData ^? _Just . Searcher.nodeLoc
        mayExpressionMode = mayModeData  ^? _Just . Searcher._ExpressionMode
        mayNodeNameMode   = mayModeData  ^? _Just . Searcher._NodeNameMode
        mayNewNodeData
            = mayExpressionMode ^? _Just . Searcher.newNodeData . _Just
        mayNewNodePosition  = mayNewNodeData    ^? _Just . Searcher.position
        getNodeTop nl       = view Node.topPosition `fmap2` getExpressionNode nl
        mayNodeTopPositionM = maybe (pure Nothing) getNodeTop maySearcherNl
        mayWorkspaceSearcherBottomM = maybe
            mayNodeTopPositionM
            (pure . Just . Node.toNodeTopPosition)
            mayNewNodePosition
        searcherRows = if isJust mayExpressionMode then 11
            else if isJust mayNodeNameMode then 2
            else 0
        bottomToTopYShift = -1 * searcherRows * searcherHeight
        bottomToTop       = move (Vector2 0 bottomToTopYShift)

    mayScreenSize     <- getScreenSize
    maySearcherBottom <- mapM translateToScreen =<< mayWorkspaceSearcherBottomM
    let maySearcherTop = bottomToTop <$> maySearcherBottom
        getCameraDelta searcherBottom searcherTop screenSize =
            let topX                = searcherTop ^. x
                topY                = searcherTop ^. y
                bottomY             = searcherBottom ^. y
                screenWidth         = screenSize ^. width
                screenHeight        = screenSize ^. height
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

    withJust mayCameraDelta $ \delta -> modifyCamera
        (invertedTranslationMatrix delta)
        (translationMatrix delta)

openWith :: Text -> Searcher.Mode -> Command State ()
openWith input mode = do
    waitingForTc <- use Global.waitingForTc
    let action   = Searcher
        inputLen = Text.length input
        searcher = Searcher.Searcher def mode def False False waitingForTc def
    begin action
    adjustCameraToSearcher mode
    modifyNodeEditor $ NodeEditor.searcher ?= searcher
    modifyInput input inputLen inputLen action
    renderIfNeeded
    Searcher.focus



updateInput :: Text -> Int -> Int -> Searcher -> Command State ()
updateInput input selectionStart selectionEnd action = do
    clearSearcherError
    let inputStream = evalDefLexer $ convert input
        searcherInput
            = if selectionStart /= selectionEnd then Searcher.RawInput input
            else if Text.null input             then Searcher.DividedInput def
            else Input.fromStream input inputStream selectionStart
        inputDivided = has Searcher._DividedInput searcherInput
        mayLambdaArgsAndEndPos = Input.findLambdaArgsAndEndOfLambdaArgs
            (convert input)
            inputStream
        lambdaArgs      = maybe mempty fst mayLambdaArgsAndEndPos
        mayLambdaEndPos = snd <$> mayLambdaArgsAndEndPos
    modifySearcher $ Searcher.input .= searcherInput
    mayMode <- view Searcher.mode `fmap2` getSearcher
    if not $ has Searcher._DividedInput searcherInput    then clearHints action
    else if has (_Just . Searcher._NodeSearcher) mayMode then do
        modifySearcher $ Searcher.mode
            . Searcher._NodeSearcher   . Searcher.modeData
            . Searcher._ExpressionMode . Searcher.argumentsNames .= lambdaArgs
        maybe
            (updateHints action)
            (\endPos -> if selectionStart < endPos
                then clearHints action
                else updateHints action)
            mayLambdaEndPos
    else updateHints action

modifyInput :: Text -> Int -> Int -> Searcher -> Command State ()
modifyInput input selectionStart selectionEnd action = do
    updateInput input selectionStart selectionEnd action
    modifySearcher $ Searcher.replaceInput .= True
    renderIfNeeded
    Searcher.setSelection selectionStart selectionEnd
    modifySearcher $ Searcher.replaceInput .= False

updateHints :: Searcher -> Command State ()
updateHints _ = localUpdateSearcherHints

clearHints :: Searcher -> Command State ()
clearHints _ = localClearSearcherHints

handleTabPressed :: Searcher -> Command State ()
handleTabPressed action = withJustM getSearcher $ \s ->
    if Text.null (s ^. Searcher.inputText)
        && s ^. Searcher.selectedPosition == 0
            then close action
            else void $ updateInputWithSelectedHint action

updateInputWithSelectedHint :: Searcher -> Command State ()
updateInputWithSelectedHint action =
    let updateDividedInput h input = do
            let mayNextChar         = input ^? Input.suffix . ix 0
                needsSpace c        = not $ elem c [' ', ')']
                trailingSpaceNeeded = maybe True needsSpace mayNextChar
                updatedQuery        = h ^. Searcher.name
                    <> if trailingSpaceNeeded then " " else mempty
                updatedInput  = input & Input.query .~ updatedQuery
                caretPosition
                    = Text.length $ input ^. Input.prefix <> updatedQuery
            modifyInput
                (convert updatedInput)
                caretPosition
                caretPosition
                action
    in withJustM getSearcher $ \s ->
        withJust (s ^. Searcher.selectedHint) $ \h -> do
            withJust (h ^? Searcher._NodeHint) includeImport
            withJust
                (s ^? Searcher.input . Input._DividedInput)
                $ updateDividedInput h

accept :: (Event -> IO ()) -> Searcher -> Command State ()
accept scheduleEvent action = do
    updateInputWithSelectedHint action
    withJustM getSearcher $ \searcher -> do
        let inputText = searcher ^. Searcher.inputText
            mode      = searcher ^. Searcher.mode
            commandSearcherAccept = execCommand action scheduleEvent inputText
            nodeSearcherAccept nl (Searcher.ExpressionMode sd) = maybe
                (setNodeExpression nl inputText)
                (\pos -> createNode (nl ^. NodeLoc.path) pos inputText False)
                $ sd ^? Searcher.newNodeData . _Just . Searcher.position
            nodeSearcherAccept nl (Searcher.NodeNameMode {})
                = renameNode nl inputText
            nodeSearcherAccept nl (Searcher.PortNameMode sd)
                = renamePort (OutPortRef nl $ sd ^. Searcher.portId) inputText
        if Text.null inputText
            then modifySearcher $ Searcher.searcherError ?= emptyInputError mode
            else case mode of
                Searcher.CommandSearcher {} -> commandSearcherAccept
                Searcher.NodeSearcher ns    -> do
                    nodeSearcherAccept
                        (ns ^. Searcher.nodeLoc)
                        (ns ^. Searcher.modeData)
                    close action

execCommand :: Searcher -> (Event -> IO ()) -> Text -> Command State ()
execCommand action scheduleEvent (convert -> input) =
    let fromCommand command = do
            liftIO $ scheduleEvent $ Shortcut $ Shortcut.Event command def
            close action
        fromOtherCommands Searcher.AddNode = modifySearcher $ do
            Searcher.selectedPosition .= def
            Searcher.mode             %= Searcher.clearHints
            Searcher.input            .= Searcher.RawInput def
            Searcher.rollbackReady    .= False
    in do
        withJust (readMaybe input) fromCommand
        withJust (readMaybe input) fromOtherCommands

close :: Searcher -> Command State ()
close _ = do
    modifyNodeEditor $ NodeEditor.searcher .= Nothing
    removeActionFromState searcherAction
    App.focus

selectNextHint :: Searcher -> Command State ()
selectNextHint _ = do
    modifySearcher $ do
        hintsLength <- use $ Searcher.hints . to length
        Searcher.selectedPosition %= min hintsLength . succ
    updateDocumentation
    withJustM getSearcher $ \s -> when
        (s ^. Searcher.selectedPosition > 0)
        clearSearcherError

selectPreviousHint :: Searcher -> Command State ()
selectPreviousHint _ = do
    withJustM getSearcher $ \s ->
        when (s ^. Searcher.selectedPosition > 0) clearSearcherError
    modifySearcher $ Searcher.selectedPosition %= max 0 . pred
    updateDocumentation

withHint :: Int -> (Searcher -> Command State ()) -> Searcher -> Command State ()
withHint entryNumber perform action = withJustM getSearcher $ \s ->
    let selected    = s ^. Searcher.selectedPosition
        hintNumber  = (entryNumber - 1) `mod` 10
        newSelected = max selected 1 + hintNumber
    in whenM (selectHint newSelected action) $ perform action

includeImport :: Match Symbol -> Command State ()
includeImport (view (Match.hint . Symbol.library) -> lib) =
    unless (lib ^. Library.imported) $ addImport (lib ^. Library.name)

selectHint :: Int -> Searcher -> Command State Bool
selectHint i _ = do
    Basic.selectHint i
    maybe
        False
        (\s -> s ^. Searcher.selectedPosition == i)
        <$> getSearcher
