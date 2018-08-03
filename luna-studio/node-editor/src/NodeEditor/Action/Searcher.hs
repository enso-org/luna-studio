{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Searcher where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Common.Report                              (warning)
import           Control.Arrow                              ((&&&))
import qualified Data.Text                                  as Text
import qualified JS.Searcher                                as Searcher
import           JS.Visualizers                             (registerVisualizerFrame)
import           Luna.Syntax.Text.Lexer                     (evalDefLexer)
import           LunaStudio.Data.Geometry                   (snap)
import           LunaStudio.Data.Matrix                     (invertedTranslationMatrix, translationMatrix)
import           LunaStudio.Data.NodeLoc                    (NodeLoc, NodePath)
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import qualified LunaStudio.Data.NodeSearcher               as NS
import           LunaStudio.Data.PortRef                    (OutPortRef)
import           LunaStudio.Data.Position                   (Position)
import           LunaStudio.Data.ScreenPosition             (move, x, y)
import           LunaStudio.Data.Size                       (height, width)
import           LunaStudio.Data.TypeRep                    (TypeRep (TCons))
import           LunaStudio.Data.Vector2                    (Vector2 (Vector2))
import           NodeEditor.Action.Basic                    (createNode, localClearSearcherHints, localUpdateSearcherHints, modifyCamera,
                                                             renameNode, renamePort, setNodeExpression, updateDocs)
import qualified NodeEditor.Action.Basic                    as Basic
import           NodeEditor.Action.Batch                    (addImport)
import           NodeEditor.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                             updateActionWithKey)
import           NodeEditor.Action.State.App                (renderIfNeeded)
import           NodeEditor.Action.State.NodeEditor         (findSuccessorPosition, getExpressionNode, getPort, getSearcher,
                                                             getSelectedNodes, modifyNodeEditor, modifySearcher)
import           NodeEditor.Action.State.Scene              (getScreenSize, translateToScreen)
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.Event.Event                     (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import           NodeEditor.React.Model.Constants           (searcherHeight, searcherWidth)
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import qualified NodeEditor.React.Model.Port                as Port
import qualified NodeEditor.React.Model.Searcher            as Searcher
import           NodeEditor.React.Model.Visualization       (Visualization (Visualization), getMdVisualizer, visualizerId)
import qualified NodeEditor.React.View.App                  as App
import           NodeEditor.State.Action                    (Action (begin, continue, end, update), Searcher (Searcher), searcherAction)
import           NodeEditor.State.Global                    (State)
import           NodeEditor.State.Global                    (visualizers)
import qualified NodeEditor.State.Global                    as Global
import qualified NodeEditor.View.NodeEditor                 as NodeEditor
import           Text.Read                                  (readMaybe)


instance Action (Command State) Searcher where
    begin    = beginActionWithKey    searcherAction
    continue = continueActionWithKey searcherAction
    update   = updateActionWithKey   searcherAction
    end      = close

mkDocVis :: Command State (Maybe Visualization)
mkDocVis = pure Nothing
    -- visId    <- getUUID
    -- iframeId <- getUUID
    -- mayVis   <- use visualizers >>= getMdVisualizer
    -- when (isNothing mayVis) $ warning "Documentation unavailable. Cannot find markdown visualizer."
    -- liftIO $ registerVisualizerFrame iframeId
    -- pure $ uncurry (Visualization visId iframeId def)
    --     . (id &&& Just . view visualizerId) <$> mayVis


emptyInputError :: Searcher.Mode -> Text
emptyInputError m = fieldName <> " cannot be empty." where
    fieldName = case m of
        Searcher.Command  {} -> "Command"
        Searcher.Node     {} -> "Node expression"
        Searcher.NodeName {} -> "Node name"
        Searcher.PortName {} -> "Port name"

clearSearcherError :: Command State ()
clearSearcherError = modifySearcher $ Searcher.searcherError .= def

editSelectedNodeExpression :: Command State ()
editSelectedNodeExpression = getSelectedNodes >>= \case
    [n] -> editExpression $ n ^. ExpressionNode.nodeLoc
    _   -> return ()

editExpression :: NodeLoc -> Command State ()
editExpression nodeLoc = do
    let getClassName n = case n ^? ExpressionNode.inPortAt [Port.Self] . Port.valueType of
            Just (TCons cn _) -> Just $ convert cn
            _                 -> Nothing
    mayN <- getExpressionNode nodeLoc
    mayDocVis <- mkDocVis
    withJust mayN $ \n ->
        openWith (n ^. ExpressionNode.code) $ Searcher.Node nodeLoc (Searcher.NodeModeInfo (getClassName n) def def mayDocVis) def

editName :: NodeLoc -> Command State ()
editName nodeLoc = do
    mayN <- getExpressionNode nodeLoc
    withJust mayN $ \n -> do
        openWith (maybe "" id $ n ^. ExpressionNode.name) $ Searcher.NodeName nodeLoc def

editPortName :: OutPortRef -> Command State ()
editPortName portRef = do
    mayP <- getPort portRef
    withJust mayP $ \p ->
        openWith (p ^. Port.name) $ Searcher.PortName portRef def

open :: Maybe Position -> Command State ()
open mayPosition = do
    (className, nn) <- getSelectedNodes >>= \case
        [n] -> do
            pos <- findSuccessorPosition n
            let (className, predPortRef) = Searcher.getPredInfo n
            return (className, Searcher.NewNode (snap pos) predPortRef)
        _   -> do
            pos <- maybe NodeEditor.getMousePosition return mayPosition
            return (def, Searcher.NewNode (snap pos) def)
    nl <- convert . ((def :: NodePath), ) <$> getUUID
    mayDocVis <- mkDocVis
    openWith "" $ Searcher.Node nl (Searcher.NodeModeInfo className (Just nn) def mayDocVis) def

openWith :: Text -> Searcher.Mode -> Command State ()
openWith input mode = do
    mayNodePosAndTop <- case mode of
        Searcher.Command  {} -> return Nothing
        Searcher.PortName {} -> return Nothing
        Searcher.NodeName nl _ -> do
            maySearcherBottom <- mapM translateToScreen . fmap (view ExpressionNode.topPosition) =<< getExpressionNode nl
            let maySearcherTop = move (Vector2 0 (-2 * searcherHeight)) <$> maySearcherBottom
            return $ (,) <$> maySearcherBottom <*> maySearcherTop
        Searcher.Node nl (Searcher.NodeModeInfo _ mayNewNodeData _ _) _ -> do
            maySearcherBottom <- mapM translateToScreen =<< case mayNewNodeData of
                Nothing -> (view ExpressionNode.topPosition) `fmap2` getExpressionNode nl
                Just (Searcher.NewNode pos _) -> return . Just $ ExpressionNode.toNodeTopPosition pos
            let maySearcherTop = move (Vector2 0 (-11 * searcherHeight)) <$> maySearcherBottom
            return $ (,) <$> maySearcherBottom <*> maySearcherTop
    mayScreenSize <- getScreenSize
    withJust ((,) <$> mayNodePosAndTop <*> mayScreenSize) $ \((searcherBottom, searcherTop), screenSize) -> do
        let overRightEdge = searcherTop ^. x + searcherWidth / 2 > screenSize ^. width
            overLeftEdge  = searcherTop ^. x - searcherWidth / 2 < 0
            overTopEdge   = searcherTop ^. y < 0
            xShift = if searcherWidth > screenSize ^. width then Nothing
                else if overRightEdge then Just $ searcherTop ^. x + searcherWidth / 2 - screenSize ^. width
                else if overLeftEdge  then Just $ searcherTop ^. x - searcherWidth / 2
                else Nothing
            yShift = if searcherBottom ^. y - searcherTop ^. y > screenSize ^. height then Just $ searcherBottom ^. y - screenSize ^. height
                else if overTopEdge then Just $ searcherTop ^. y
                else Nothing
            mayDelta = if isNothing xShift && isNothing yShift then Nothing else Just $ Vector2 (fromMaybe def xShift) (fromMaybe def yShift)
        withJust mayDelta $ \delta ->
            modifyCamera (invertedTranslationMatrix delta) (translationMatrix delta)
    let action      = Searcher
        inputLen    = Text.length input
        targetField = case mode of
                Searcher.NodeName{} -> Just "name"
                Searcher.Node{}     -> Just "expression"
                _                   -> Nothing
    begin action
    waitingForTc <- use Global.waitingForTc
    modifyNodeEditor $ NodeEditor.searcher ?= Searcher.Searcher 0 mode def False def False waitingForTc def targetField
    modifyInput input inputLen inputLen action
    renderIfNeeded --TODO[basegl] Remove
    -- Searcher.focus --TODO[basegl] Remove

updateInput :: Text -> Int -> Int -> Searcher -> Command State ()
updateInput input selectionStart selectionEnd action = do
    clearSearcherError
    let inputStream = evalDefLexer $ convert input
        newInput
            | selectionStart /= selectionEnd = Searcher.Raw input
            | Text.null input = Searcher.Divided $ Searcher.DividedInput def def def
            | otherwise = Searcher.fromStream input inputStream selectionStart
    modifySearcher $ Searcher.input .= newInput
    m <- fmap2 (view Searcher.mode) getSearcher
    if      isNothing $ newInput ^? Searcher._Divided then clearHints action
    else if isJust $ maybe def (^? Searcher._Node) m  then
        case Searcher.findLambdaArgsAndEndOfLambdaArgs (convert input) inputStream of
            Nothing             -> do
                modifySearcher $ Searcher.mode %= Searcher.updateNodeArgs []
                updateHints action
            Just (args, endPos) -> do
                modifySearcher $ Searcher.mode %= Searcher.updateNodeArgs (convert args)
                if selectionStart < endPos then clearHints action else updateHints action
    else updateHints action

modifyInput :: Text -> Int -> Int -> Searcher -> Command State ()
modifyInput input selectionStart selectionEnd action = do
    updateInput input selectionStart selectionEnd action
    modifySearcher $ Searcher.replaceInput   .= True --TODO[basegl] Remove
    modifySearcher $ Searcher.inputSelection .= Just (selectionStart, selectionEnd)
    renderIfNeeded --TODO[basegl] Remove
    modifySearcher $ Searcher.inputSelection .= Nothing
    Searcher.setSelection selectionStart selectionEnd --TODO[basegl] Remove
    modifySearcher $ Searcher.replaceInput .= False --TODO[basegl] Remove

updateHints :: Searcher -> Command State ()
updateHints _ = localUpdateSearcherHints

clearHints :: Searcher -> Command State ()
clearHints _ = localClearSearcherHints

handleTabPressed :: Searcher -> Command State ()
handleTabPressed action = withJustM getSearcher $ \s ->
    if Text.null (s ^. Searcher.inputText) && s ^. Searcher.selected == 0
        then close action
        else void $ updateInputWithSelectedHint action

updateInputWithSelectedHint :: Searcher -> Command State Bool
updateInputWithSelectedHint action = getSearcher >>= maybe (return False) updateWithSearcher where
    updateWithSearcher s = if s ^. Searcher.selected == 0 then return True else do
        let mayMatch        = s ^. Searcher.selectedMatch
            mayExpr         = (view NS.name) <$> mayMatch
            mayDividedInput = s ^? Searcher.input . Searcher._Divided
        withJust mayMatch includeImport
        withJust ((,) <$> mayExpr <*> mayDividedInput) $ \(expr, divInput) -> do
            let divInput' = divInput & Searcher.query .~ expr'
                lastChar = divInput ^? Searcher.suffix . ix 0
                expr'    = if lastChar == Just ' '
                           || lastChar == Just ')'
                           then expr else expr <> " "
                newInput = Searcher.toText $ Searcher.Divided divInput'
                caretPos = Text.length (divInput' ^. Searcher.prefix) + Text.length expr'
            modifyInput newInput caretPos caretPos action
        return $ isJust mayExpr && isJust mayDividedInput

accept :: (Event -> IO ()) -> Searcher -> Command State ()
accept scheduleEvent action = whenM (updateInputWithSelectedHint action) $
    withJustM getSearcher $ \searcher -> do
        let inputText = searcher ^. Searcher.inputText
            mode      = searcher ^. Searcher.mode
        if Text.null inputText
            then modifySearcher $ Searcher.searcherError ?= emptyInputError mode
            else case mode of
                Searcher.Command                                             _ -> execCommand action scheduleEvent $ convert inputText
                Searcher.Node     nl (Searcher.NodeModeInfo _ (Just nn) _ _) _ -> createNode (nl ^. NodeLoc.path) (nn ^. Searcher.position) inputText False >> close action
                Searcher.Node     nl _                                       _ -> setNodeExpression nl inputText >> close action
                Searcher.NodeName nl                                         _ -> renameNode nl inputText >> close action
                Searcher.PortName portRef                                    _ -> renamePort portRef inputText >> close action

execCommand :: Searcher -> (Event -> IO ()) -> String -> Command State ()
execCommand action scheduleEvent inputText = case readMaybe inputText of
    Just command -> do
        liftIO $ scheduleEvent $ Shortcut $ Shortcut.Event command def
        close action
    Nothing -> case readMaybe inputText of
        Just Searcher.AddNode -> modifySearcher $ do
            Searcher.selected .= def
            Searcher.mode     %= (\(Searcher.Node nl nmi _) -> Searcher.Node nl nmi def)
            Searcher.input    .= Searcher.Raw def
            Searcher.rollbackReady .= False
        Nothing -> return ()

close :: Searcher -> Command State ()
close _ = do
    modifyNodeEditor $ NodeEditor.searcher .= Nothing
    removeActionFromState searcherAction
    App.focus

selectNextHint :: Searcher -> Command State ()
selectNextHint _ = do
    modifySearcher $ use (Searcher.hints . to length) >>= \hintsLen ->
        Searcher.selected %= min hintsLen . succ
    updateDocs
    withJustM getSearcher $ \s -> when (s ^. Searcher.selected > 0) clearSearcherError

selectPreviousHint :: Searcher -> Command State ()
selectPreviousHint _ = do
    withJustM getSearcher $ \s -> when (s ^. Searcher.selected > 0) clearSearcherError
    modifySearcher $ Searcher.selected %= max 0 . pred
    updateDocs

acceptWithHint :: (Event -> IO ()) -> Int -> Searcher -> Command State ()
acceptWithHint scheduleEvent hintNum' action = let hintNum = (hintNum' - 1) `mod` 10 in
    withJustM (view Searcher.selected `fmap2` getSearcher) $ \selected ->
        whenM (selectHint (max selected 1 + hintNum) action) $ accept scheduleEvent action

updateInputWithHint :: Int -> Searcher -> Command State ()
updateInputWithHint hintNum' action = let hintNum = (hintNum' - 1) `mod` 10 in
    withJustM (view Searcher.selected `fmap2` getSearcher) $ \selected ->
        whenM_ (selectHint (max selected 1 + hintNum) action) $
            updateInputWithSelectedHint action

includeImport :: NS.Match -> Command State ()
includeImport m = withJust (m ^. NS.importInfo) $ \ii -> unless (ii ^. NS.imported) . addImport $ ii ^. NS.importName

selectHint :: Int -> Searcher -> Command State Bool
selectHint i _ = do
    Basic.selectHint i
    maybe False ((i ==) . view Searcher.selected) <$> getSearcher
