{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Searcher where

import           Common.Prelude
import qualified Data.Text                                  as Text
import qualified JS.GoogleAnalytics                         as GA
import qualified JS.Searcher                                as Searcher
import           Luna.Syntax.Text.Lexer                     (runGUILexer)
import           LunaStudio.Data.NodeLoc                    (NodeLoc, NodePath)
import           LunaStudio.Data.Geometry                   (snap)
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import           LunaStudio.Data.PortRef                    (OutPortRef (OutPortRef))
import           LunaStudio.Data.TypeRep                    (TypeRep (TCons))
import           NodeEditor.Action.Basic                    (createNode, localClearSearcherHints, localUpdateSearcherHints, renameNode,
                                                             renamePort, setNodeExpression)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                             updateActionWithKey)
import           NodeEditor.Action.State.App                (renderIfNeeded)
import           NodeEditor.Action.State.NodeEditor         (findSuccessorPosition, getExpressionNode, getPort, getSearcher,
                                                             getSelectedNodes, getSelectedNodes, modifyNodeEditor, modifySearcher)
import           NodeEditor.Action.State.Scene              (translateToWorkspace)
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.Event.Event                     (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import qualified NodeEditor.React.Model.Port                as Port
import qualified NodeEditor.React.Model.Searcher            as Searcher
import qualified NodeEditor.React.View.App                  as App
import           NodeEditor.State.Action                    (Action (begin, continue, end, update), Searcher (Searcher), searcherAction)
import           NodeEditor.State.Global                    (State)
import qualified NodeEditor.State.Global                    as Global
import qualified NodeEditor.State.UI                        as UI
import           Text.Read                                  (readMaybe)


instance Action (Command State) Searcher where
    begin    = beginActionWithKey    searcherAction
    continue = continueActionWithKey searcherAction
    update   = updateActionWithKey   searcherAction
    end      = close


editExpression :: NodeLoc -> Command State ()
editExpression nodeLoc = do
    let getClassName n = case n ^? ExpressionNode.inPortAt [Port.Self] . Port.valueType of
            Just (TCons cn _) -> Just $ convert cn
            _                 -> Nothing
    mayN <- getExpressionNode nodeLoc
    withJust mayN $ \n -> do
        openWith (n ^. ExpressionNode.expression) $ Searcher.Node nodeLoc (Searcher.NodeModeInfo (getClassName n) def def) def

editName :: NodeLoc -> Command State ()
editName nodeLoc = do
    mayN <- getExpressionNode nodeLoc
    withJust mayN $ \n -> do
        openWith (maybe "" id $ n ^. ExpressionNode.name) $ Searcher.NodeName nodeLoc def

editPortName :: OutPortRef -> Command State ()
editPortName portRef = do
    mayP <- getPort portRef
    withJust mayP $ \p -> do
        openWith (p ^. Port.name) $ Searcher.PortName portRef def

open :: Command State ()
open = do
    (className, nn) <- getSelectedNodes >>= \case
        [n] -> do
            pos <- findSuccessorPosition n
            let mayP        = listToMaybe $ ExpressionNode.outPortsList n
                className   = case view Port.valueType <$> mayP of
                    Just (TCons cn _) -> Just $ convert cn
                    _                 -> Nothing
                predPortRef = OutPortRef (n ^. ExpressionNode.nodeLoc) . view Port.portId <$> mayP
            return $ (className, Searcher.NewNode (snap pos) predPortRef)
        _   -> do
            pos <- translateToWorkspace =<< use (Global.ui . UI.mousePos)
            return $ (def, Searcher.NewNode (snap pos) def)
    nl <- convert . ((def :: NodePath), ) <$> getUUID
    openWith "" $ Searcher.Node nl (Searcher.NodeModeInfo className (Just nn) def) def

openWith :: Text -> Searcher.Mode -> Command State ()
openWith input mode = do
    let action   = Searcher
        inputLen = Text.length input
    begin action
    GA.sendEvent GA.NodeSearcher
    modifyNodeEditor $ NodeEditor.searcher ?= Searcher.Searcher 0 mode def False False
    updateInput input inputLen inputLen action
    forceSearcherInputUpdate
    liftIO Searcher.focus


updateInput :: Text -> Int -> Int -> Searcher -> Command State ()
updateInput input selectionStart selectionEnd action = do
    let inputStream = runGUILexer $ convert input
        newInput    = if      selectionStart /= selectionEnd
                          then Searcher.Raw input
                      else if Text.null input
                          then Searcher.Divided $ Searcher.DividedInput def def def
                          else Searcher.fromStream input inputStream selectionStart
    modifySearcher $ Searcher.input .= newInput
    m <- fmap2 (view Searcher.mode) $ getSearcher
    if      isNothing $ newInput ^? Searcher._Divided then clearHints action
    else if isJust $ maybe def (^? Searcher._Node) m  then do
        case Searcher.findLambdaArgsAndEndOfLambdaArgs inputStream of
            Nothing             -> do
                modifySearcher $ Searcher.mode %= Searcher.updateNodeArgs []
                updateHints action
            Just (args, endPos) -> do
                modifySearcher $ Searcher.mode %= Searcher.updateNodeArgs (convert args)
                if selectionStart < endPos then clearHints action else do updateHints action
    else updateHints action

updateHints :: Searcher -> Command State ()
updateHints _ = localUpdateSearcherHints

clearHints :: Searcher -> Command State ()
clearHints _ = localClearSearcherHints

handleTabPressed :: Searcher -> Command State ()
handleTabPressed action = withJustM getSearcher $ \s ->
    if Text.null (s ^. Searcher.inputText) && s ^. Searcher.selected == 0
        then close action
        else updateInputWithSelectedHint action >> forceSearcherInputUpdate

updateInputWithSelectedHint :: Searcher -> Command State Bool
updateInputWithSelectedHint action = getSearcher >>= maybe (return False) updateWithSearcher where
    updateWithSearcher s = if s ^. Searcher.selected == 0 then return True else do
        let mayExpr         = s ^. Searcher.selectedExpression
            mayDividedInput = s ^? Searcher.input . Searcher._Divided
        withJust ((,) <$> mayExpr <*> mayDividedInput) $ \(expr, divInput) -> do
            let expr'    = if divInput ^? Searcher.suffix . ix 0 == Just ' ' then expr else expr <> " "
                newInput = Searcher.toText . Searcher.Divided $ divInput & Searcher.query .~ expr'
                caretPos = Text.length newInput
            updateInput newInput caretPos caretPos action
        return $ isJust mayExpr && isJust mayDividedInput

accept :: (Event -> IO ()) -> Searcher -> Command State ()
accept scheduleEvent action = whenM (updateInputWithSelectedHint action) $
    withJustM getSearcher $ \searcher -> do
        let expression = searcher ^. Searcher.inputText
        case searcher ^. Searcher.mode of
            Searcher.Command                                           _ -> execCommand action scheduleEvent $ convert expression
            Searcher.Node     nl (Searcher.NodeModeInfo _ (Just nn) _) _ -> createNode (nl ^. NodeLoc.path) (nn ^. Searcher.position) expression >> close action
            Searcher.Node     nl _                                     _ -> setNodeExpression nl expression >> close action
            Searcher.NodeName nl                                       _ -> renameNode nl expression >> close action
            Searcher.PortName portRef                                  _ -> renamePort portRef expression >> close action

execCommand :: Searcher -> (Event -> IO ()) -> String -> Command State ()
execCommand action scheduleEvent expression = case readMaybe expression of
    Just command -> do
        liftIO $ scheduleEvent $ Shortcut $ Shortcut.Event command def
        close action
    Nothing -> case readMaybe expression of
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
    liftIO App.focus

selectNextHint :: Searcher -> Command State ()
selectNextHint _ = modifySearcher $ do
    hintsLen <- use Searcher.resultsLength
    Searcher.selected %= \p -> (p + 1) `mod` (hintsLen + 1)

selectPreviousHint :: Searcher -> Command State ()
selectPreviousHint _ = modifySearcher $ do
    hintsLen <- use Searcher.resultsLength
    Searcher.selected %= \p -> (p - 1) `mod` (hintsLen + 1)

selectHint :: Int -> Searcher -> Command State Bool
selectHint i _ = do
    mayHintsLen <- fmap2 (view Searcher.resultsLength) getSearcher
    case mayHintsLen of
        Nothing       -> return False
        Just hintsLen -> if i < 0 || i > hintsLen then return False else do
            modifySearcher $ Searcher.selected .= i
            return True

acceptHint :: (Event -> IO ()) -> Int -> Searcher -> Command State ()
acceptHint scheduleEvent hintNum action =
    whenM (selectHint hintNum action) $ accept scheduleEvent action


forceSearcherInputUpdate :: Command State ()
forceSearcherInputUpdate = do
    modifySearcher $ Searcher.replaceInput .= True
    renderIfNeeded
    modifySearcher $ Searcher.replaceInput .= False

-- tryRollback :: Searcher -> Command State ()
-- tryRollback _ = do
--     withJustM getSearcher $ \searcher -> do
--        when (Text.null (searcher ^. Searcher.inputText)
--          && (searcher ^. Searcher.isNode)
--          && (searcher ^. Searcher.rollbackReady)) $
--             modifySearcher $ do
--                 Searcher.rollbackReady .= False
--                 Searcher.selected      .= def
--                 Searcher.mode          .= Searcher.Command def
--                 Searcher.input         .= Searcher.Raw def
--
-- enableRollback :: Searcher -> Command State ()
-- enableRollback _ = modifySearcher $
--     Searcher.rollbackReady .= True
