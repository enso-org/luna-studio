module NodeEditor.Action.Searcher where

import Common.Prelude

import qualified Data.Char                                  as Char
import qualified Data.Text                                  as Text
import qualified JS.Searcher                                as Searcher
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import qualified LunaStudio.Data.PortRef                    as PortRef
import qualified LunaStudio.Data.Searcher.Hint.Library      as Library
import qualified LunaStudio.Data.TypeRep                    as TypeRep
import qualified NodeEditor.Action.Basic                    as Basic
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import qualified NodeEditor.React.Model.Port                as Port
import qualified NodeEditor.React.Model.Searcher            as Searcher
import qualified NodeEditor.React.Model.Searcher.Hint       as Hint
import qualified NodeEditor.React.Model.Searcher.Hint.Node  as NodeHint
import qualified NodeEditor.React.Model.Searcher.Input      as Input
import qualified NodeEditor.React.Model.Searcher.Mode       as Mode
import qualified NodeEditor.React.Model.Searcher.Mode.Node  as NodeMode
import qualified NodeEditor.React.Model.Searcher.UndoRedo   as UndoRedo
import qualified NodeEditor.React.Model.Visualization       as Visualization
import qualified NodeEditor.React.View.App                  as App
import qualified NodeEditor.State.Global                    as Global
import qualified NodeEditor.State.UI                        as UI
import qualified Searcher.Data.Class                        as SearcherData
import qualified Searcher.Data.Result                       as Result

import Common.Action.Command                (Command)
import Common.Report                        (warning)
import JS.Visualizers                       (registerVisualizerFrame)
import Luna.Syntax.Text.Lexer               (evalDefLexer)
import LunaStudio.Data.Geometry             (snap)
import LunaStudio.Data.Matrix               (invertedTranslationMatrix,
                                             translationMatrix)
import LunaStudio.Data.NodeLoc              (NodeLoc, NodePath)
import LunaStudio.Data.PortRef              (OutPortRef (OutPortRef))
import LunaStudio.Data.Position             (Position)
import LunaStudio.Data.ScreenPosition       (move, x, y)
import LunaStudio.Data.Size                 (height, width)
import LunaStudio.Data.Vector2              (Vector2 (Vector2))
import NodeEditor.Action.Basic              (clearHints, createNode,
                                             modifyCamera, renameNode,
                                             renamePort, setNodeExpression,
                                             updateDocumentation, updateHints)
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
import NodeEditor.React.Model.Visualization (RunningVisualization (RunningVisualization),
                                             VisualizerProperties (VisualizerProperties),
                                             getMdVisualizer)
import NodeEditor.State.Action              (Action (begin, continue, end, update),
                                             Searcher (Searcher),
                                             searcherAction)
import NodeEditor.State.Global              (State, visualizers)
import Text.Read                            (readMaybe)

instance Action (Command State) Searcher where
    begin    = beginActionWithKey    searcherAction
    continue = continueActionWithKey searcherAction
    update   = updateActionWithKey   searcherAction
    end      = close

unavailableDocumentationMsg :: String
unavailableDocumentationMsg
    = "Documentation unavailable. Cannot find markdown visualizer."

mkDocumentationVisualization :: Command State (Maybe RunningVisualization)
mkDocumentationVisualization = do
    mayVis <- use visualizers >>= getMdVisualizer
    let mayVisId :: Maybe Visualization.VisualizerId
        mayVisId   = view Visualization.visualizerId <$> mayVis
        mayVisProp :: Maybe Visualization.VisualizerProperties
        mayVisProp = flip VisualizerProperties mayVisId <$> mayVis
    when (isNothing mayVisProp) $ warning unavailableDocumentationMsg
    forM mayVisProp $ \vp -> getUUID >>= \uuid -> do
        liftIO $ registerVisualizerFrame uuid
        pure $ RunningVisualization uuid def vp

editSelectedNodeExpression :: Command State ()
editSelectedNodeExpression = getSelectedNodes >>= \case
    [n] -> editExpression $ n ^. Node.nodeLoc
    _   -> pure ()

editExpression :: NodeLoc -> Command State ()
editExpression nl =
    let getClassName n = convert <$> n ^? Node.inPortAt [Port.Self]
            . Port.valueType . TypeRep._TCons . _1
        mkExpressionData n = NodeMode.ExpressionMode $ NodeMode.Expression
            Nothing
            (getClassName n)
            mempty
        mkExpressionNodesDataM n = do
            docVis <- mkDocumentationVisualization
            pure $ NodeMode.Node nl docVis $ mkExpressionData n
        mkExpressionSearcherModeM n
            = Mode.Node <$> mkExpressionNodesDataM n
    in withJustM (getExpressionNode nl) $ \n -> openWith
        (n ^. Node.code)
        =<< mkExpressionSearcherModeM n

editName :: NodeLoc -> Command State ()
editName nl = withJustM (getExpressionNode nl) $ \n -> openWith
    (maybe mempty id $ n ^. Node.name)
    $ Mode.Node $ NodeMode.Node nl def NodeMode.NodeNameMode

editPortName :: OutPortRef -> Command State ()
editPortName portRef = withJustM (getPort portRef) $ \p -> openWith
    (p ^. Port.name)
    $ Mode.Node $ NodeMode.Node
        (portRef ^. PortRef.nodeLoc)
        def
        (NodeMode.PortNameMode
            $ NodeMode.PortName $ portRef ^. PortRef.srcPortId)

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
        newNodeDataM = NodeMode.New
            <$> positionM
            <*> mayConnectedPortRefM
        getPortClassName p = convert
            <$> p ^? Port.valueType . TypeRep._TCons . _1
        mayClassNameM = maybe mempty getPortClassName <$> mayConnectedPortM
        expressionModeM = NodeMode.ExpressionMode .:. NodeMode.Expression
            <$> (Just <$> newNodeDataM)
            <*> mayClassNameM
            <*> pure mempty
        mkNodesDataM nl = NodeMode.Node
            nl
            <$> mkDocumentationVisualization
            <*> expressionModeM
        mkNodeSearcherM nl = Mode.Node <$> mkNodesDataM nl

    nl <- convert . ((def :: NodePath), ) <$> getUUID
    openWith mempty =<< mkNodeSearcherM nl

adjustCameraToSearcher :: Mode.Mode -> Command State ()
adjustCameraToSearcher mode = do
    let mayNodesData      = mode         ^? Mode._Node
        mayModeData       = mayNodesData ^? _Just . NodeMode.mode
        maySearcherNl     = mayNodesData ^? _Just . NodeMode.nodeLoc
        mayExpressionMode = mayModeData  ^? _Just . NodeMode._ExpressionMode
        mayNodeNameMode   = mayModeData  ^? _Just . NodeMode._NodeNameMode
        mayNewNodeData
            = mayExpressionMode ^? _Just . NodeMode.newNode . _Just
        mayNewNodePosition  = mayNewNodeData    ^? _Just . NodeMode.position
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

openWith :: Text -> Mode.Mode -> Command State ()
openWith input mode = do
    let action   = Searcher
        inputLen = Text.length input
        inpState = UndoRedo.InputState input inputLen inputLen
        undoRedo = UndoRedo.mk inpState
        searcher = Searcher.Searcher def False def def mode False undoRedo
    begin action
    s <- modifySearcher $ (: []) <$> use id
    adjustCameraToSearcher mode
    modifyNodeEditor $ NodeEditor.searcher ?= searcher
    setInput input inputLen inputLen action
    renderIfNeeded
    Searcher.focus


updateInput :: Text -> Int -> Int -> Searcher -> Command State ()
updateInput input selectionStart selectionEnd action = do
    let inputStream = evalDefLexer $ convert input
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
    modifySearcher $ Searcher.input .= searcherInput
    mayMode <- view Searcher.mode `fmap2` getSearcher
    if not $ has Input._DividedInput searcherInput then clearHints
    else if has (_Just . Mode._Node) mayMode then do
        modifySearcher $ Searcher.mode
            . Mode._Node . NodeMode.mode
            . NodeMode._ExpressionMode . NodeMode.arguments .= lambdaArgs
        maybe
            updateHints
            (\endPos -> if selectionStart < endPos
                then clearHints
                else updateHints)
            mayLambdaEndPos
    else updateHints

openParen :: Searcher -> Command State ()
openParen sKey = do
    (selStart, selEnd) <- Searcher.getSelection
    setUndoSelection selStart selEnd
    withJustM getSearcher $ \s -> do
        let input = convert $ s ^. Searcher.input
            (beg, rest) = Text.splitAt selStart input
            (rel, end)  = Text.splitAt (selEnd - selStart) rest
            closeBefore = [')', ']', ':']
            nextCharIsSpace = case Text.uncons end of
                Nothing     -> True
                Just (c, _) -> Char.isSpace c || elem c closeBefore
            shouldClose = nextCharIsSpace || selStart /= selEnd
            rel' = "(" <> rel <> (if shouldClose then ")" else "")
            newInput = beg <> rel' <> end
        modifyInput newInput (selStart + 1) (selEnd + 1) sKey

closeParen :: Searcher -> Command State ()
closeParen sKey = do
    (selStart, selEnd) <- Searcher.getSelection
    setUndoSelection selStart selEnd
    withJustM getSearcher $ \s -> do
        let input = convert $ s ^. Searcher.input
            (beg, rest) = Text.splitAt selStart input
            (rel, end)  = Text.splitAt (selEnd - selStart) rest
            newInput    = case Text.uncons end of
                Just (')', _) -> if selStart == selEnd
                    then beg <> end
                    else beg <> ")" <> end
                _ -> beg <> ")" <> end
        modifyInput newInput (selEnd + 1) (selEnd + 1) sKey

backspace :: Searcher -> Command State ()
backspace sKey = do
    (selStart, selEnd) <- Searcher.getSelection
    setUndoSelection selStart selEnd
    withJustM getSearcher $ \s -> do
        let input = convert $ s ^. Searcher.input
            (removeStart, removeEnd) = if selStart == selEnd
                then (selStart - 1, selStart)
                else (selStart, selEnd)
            (beg, rest) = Text.splitAt removeStart input
            (rem, end)  = Text.splitAt (removeEnd - removeStart) rest
            newInput    = case (rem, Text.uncons end) of
                ("(", Just (')', t)) -> if selStart == selEnd
                    then beg <> t
                    else beg <> end
                _ -> beg <> end
        modifyInput newInput removeStart removeStart sKey


setInput :: Text -> Int -> Int -> Searcher -> Command State ()
setInput input selectionStart selectionEnd action = do
    updateInput input selectionStart selectionEnd action
    modifySearcher $ Searcher.replaceInput .= True
    renderIfNeeded
    Searcher.setSelection selectionStart selectionEnd
    modifySearcher $ Searcher.replaceInput .= False

setUndoSelection :: Int -> Int -> Command State ()
setUndoSelection selStart selEnd = do
    modifySearcher $ Searcher.undoRedo %= UndoRedo.setSelection selStart selEnd

updateSelection :: Searcher -> Command State ()
updateSelection _ = do
    (selStart, selEnd) <- Searcher.getSelection
    setUndoSelection selStart selEnd

modifyInput :: Text -> Int -> Int -> Searcher -> Command State ()
modifyInput input selStart selEnd action = do
    let inp = UndoRedo.InputState input selStart selEnd
    modifySearcher $ Searcher.undoRedo %= UndoRedo.setInput inp
    setInput input selStart selEnd action

undo :: Searcher -> Command State ()
undo searcherTag = withJustM getSearcher $ \searcher -> do
    let newState = UndoRedo.undo $ searcher ^. Searcher.undoRedo
        newInput = newState ^. UndoRedo.currentInput
    modifySearcher $ Searcher.undoRedo .= newState
    setInput (newInput ^. UndoRedo.text)
             (newInput ^. UndoRedo.selectionStart)
             (newInput ^. UndoRedo.selectionEnd)
             searcherTag

redo :: Searcher -> Command State ()
redo searcherTag = withJustM getSearcher $ \searcher -> do
    let newState = UndoRedo.redo $ searcher ^. Searcher.undoRedo
        newInput = newState ^. UndoRedo.currentInput
    modifySearcher $ Searcher.undoRedo .= newState
    setInput (newInput ^. UndoRedo.text)
             (newInput ^. UndoRedo.selectionStart)
             (newInput ^. UndoRedo.selectionEnd)
             searcherTag

handleTabPressed :: Searcher -> Command State ()
handleTabPressed action = withJustM getSearcher $ \s ->
    if Text.null (s ^. Searcher.inputText)
        && isNothing (s ^. Searcher.selectedPosition)
            then close action
            else void $ updateInputWithSelectedHint action

updateInputWithSelectedHint :: Searcher -> Command State ()
updateInputWithSelectedHint action =
    let updateDividedInput h input = do
            let mayNextChar         = input ^? Input.suffix . ix 0
                needsSpace c        = notElem c [' ', ')']
                trailingSpaceNeeded = maybe True needsSpace mayNextChar
                updatedQuery        = h ^. SearcherData.text
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
        withJust (s ^. Searcher.selectedResult) $ \h -> do
            withJust (h ^? Result.hint . Hint._Node) includeImport
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
            nodeSearcherAccept nl (NodeMode.ExpressionMode sd) = maybe
                (setNodeExpression nl inputText)
                (\pos -> createNode (nl ^. NodeLoc.path) pos inputText False)
                $ sd ^? NodeMode.newNode . _Just . NodeMode.position
            nodeSearcherAccept nl NodeMode.NodeNameMode {}
                = renameNode nl inputText
            nodeSearcherAccept nl (NodeMode.PortNameMode sd)
                = renamePort (OutPortRef nl $ sd ^. NodeMode.portId) inputText
        if Text.null inputText
            then pure ()
            else case mode of
                Mode.Command {} -> commandSearcherAccept
                Mode.Node ns    -> do
                    nodeSearcherAccept
                        (ns ^. NodeMode.nodeLoc)
                        (ns ^. NodeMode.mode)
                    close action

execCommand :: Searcher -> (Event -> IO ()) -> Text -> Command State ()
execCommand action scheduleEvent (convert -> input) =
    let fromCommand command = do
            liftIO $ scheduleEvent $ Shortcut $ Shortcut.Event command def
            close action
    in withJust (readMaybe input) fromCommand

close :: Searcher -> Command State ()
close _ = do
    modifyNodeEditor $ NodeEditor.searcher .= Nothing
    removeActionFromState searcherAction
    App.focus

selectNextHint :: Searcher -> Command State ()
selectNextHint s = do
    Basic.selectNextHint
    updateDocumentation

selectPreviousHint :: Searcher -> Command State ()
selectPreviousHint s = do
    Basic.selectPreviousHint
    updateDocumentation

withHint :: Int -> (Searcher -> Command State ()) -> Searcher
         -> Command State ()
withHint entryNumber perform action = withJustM getSearcher $ \s ->
    let selected    = case s ^. Searcher.selectedPosition of
            Nothing -> 0
            Just t  -> t + 1
        hintNumber  = (entryNumber - 1) `mod` 10
        newSelected = max selected 1 + hintNumber
    in whenM (selectHint newSelected action) $ perform action

includeImport :: NodeHint.Node -> Command State ()
includeImport hint = let
    lib        = hint ^. NodeHint.library
    isImported = lib  ^. Library.imported
    name       = lib  ^. Library.name
    isLocal    = name == NodeHint.localFunctionsLibraryName
    in unless (isImported || isLocal) $ do
        Global.searcherDatabase %= NodeHint.importLibrary name
        addImport name

selectHint :: Int -> Searcher -> Command State Bool
selectHint i _ = do
    let translated = if i == 0 then Nothing else Just $ i - 1
    Basic.selectHint translated
    maybe
        False
        (\s -> s ^. Searcher.selectedPosition == translated)
        <$> getSearcher
