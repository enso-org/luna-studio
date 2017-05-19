{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Searcher where

import qualified Data.Map                           as Map
import           Data.Monoid                        (All (All), getAll)
import qualified Data.Text                          as Text
import           Text.Read                          (readMaybe)

import           Common.Prelude
import           Data.ScreenPosition                (ScreenPosition)
import qualified JS.GoogleAnalytics                 as GA
import qualified JS.Searcher                        as Searcher
import           LunaStudio.Data.NodeLoc            (NodeLoc, NodePath)
import qualified LunaStudio.Data.NodeLoc            as NodeLoc
import           LunaStudio.Data.PortRef            (OutPortRef)
import qualified LunaStudio.Data.PortRef            as PortRef
import           LunaStudio.Data.Position           (Position)
import           NodeEditor.Action.Basic            (createNode, renameNode, renamePort, setNodeExpression)
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.State.Action     (beginActionWithKey, continueActionWithKey, removeActionFromState, updateActionWithKey)
import           NodeEditor.Action.State.App        (renderIfNeeded)
import           NodeEditor.Action.State.NodeEditor (findSuccessorPosition, getSearcher, getSelectedNodes, getSelectedNodes,
                                                     modifyNodeEditor, modifySearcher)
import           NodeEditor.Action.State.Scene      (translateToScreen, translateToWorkspace)
import           NodeEditor.Action.UUID             (getUUID)
import           NodeEditor.Event.Event             (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut          as Shortcut
import qualified NodeEditor.React.Model.NodeEditor  as NodeEditor
import qualified NodeEditor.React.Model.Searcher    as Searcher
import qualified NodeEditor.React.View.App          as App
import           NodeEditor.State.Action            (Action (begin, continue, end, update), Searcher (Searcher), searcherAction)
import           NodeEditor.State.Global            (State)
import qualified NodeEditor.State.Global            as Global
import qualified NodeEditor.State.UI                as UI
import           Text.ScopeSearcher.Item            (Item (..), Items)
import qualified Text.ScopeSearcher.Scope           as Scope


instance Action (Command State) Searcher where
    begin    = beginActionWithKey    searcherAction
    continue = continueActionWithKey searcherAction
    update   = updateActionWithKey   searcherAction
    end      = close


data OtherCommands = AddNode
                   deriving (Bounded, Enum, Eq, Generic, Read, Show)

open :: Command State ()
open = do
    pos <- getSelectedNodes >>= \case
        [n] -> findSuccessorPosition n
        _   -> translateToWorkspace =<< use (Global.ui . UI.mousePos)
    nl <- convert . ((def :: NodePath), ) <$> getUUID
    openWith (Searcher.Node nl (Just pos) def)

positionDelta :: Double
positionDelta = 100

openWith :: Searcher.Mode -> Command State ()
openWith mode = do
    begin Searcher
    GA.sendEvent GA.NodeSearcher
    modifyNodeEditor $ NodeEditor.searcher ?= Searcher.Searcher 0 mode def False False
    renderIfNeeded
    liftIO Searcher.focus

close :: Searcher -> Command State ()
close _ = do
    modifyNodeEditor $ NodeEditor.searcher .= Nothing
    removeActionFromState searcherAction
    liftIO App.focus

moveDown :: Searcher -> Command State ()
moveDown _ = modifySearcher $ do
    items <- use Searcher.resultsLength
    unless (items == 0) $
        Searcher.selected %= \p -> (p - 1) `mod` (items + 1)

moveUp :: Searcher -> Command State ()
moveUp _ = modifySearcher $ do
    items <- use Searcher.resultsLength
    unless (items == 0) $
        Searcher.selected %= \p -> (p + 1) `mod` (items + 1)

tryRollback :: Searcher -> Command State ()
tryRollback _ = do
    withJustM getSearcher $ \searcher -> do
       when (Text.null (searcher ^. Searcher.input)
         && (searcher ^. Searcher.isNode)
         && (searcher ^. Searcher.rollbackReady)) $
            modifySearcher $ do
                Searcher.rollbackReady .= False
                Searcher.selected      .= def
                Searcher.mode          .= Searcher.Command def
                Searcher.input         .= def

enableRollback :: Searcher -> Command State ()
enableRollback _ = modifySearcher $
    Searcher.rollbackReady .= True

accept :: (Event -> IO ()) -> Searcher -> Command State ()
accept scheduleEvent action = do
    withJustM getSearcher $ \searcher -> do
        let expression = searcher ^. Searcher.selectedExpression
        case searcher ^. Searcher.mode of
            Searcher.Command                _ -> execCommand action scheduleEvent $ convert expression
            Searcher.Node     nl (Just pos) _ -> createNode (nl ^. NodeLoc.path) pos expression >> close action
            Searcher.Node     nl _          _ -> setNodeExpression nl expression >> close action
            Searcher.NodeName nl            _ -> renameNode nl expression >> close action
            Searcher.PortName portRef       _ -> renamePort portRef expression >> close action

openEditExpression :: NodeLoc -> Text -> Command State ()
openEditExpression nodeLoc expr = do
    openWith $ Searcher.Node nodeLoc def def
    continue $ querySearch expr

openEditName :: NodeLoc -> Maybe Text -> Command State ()
openEditName nodeLoc mayName = do
    openWith $ Searcher.NodeName nodeLoc def
    continue $ querySearch $ maybe "" id mayName

openEditPortName :: OutPortRef -> Text -> Command State ()
openEditPortName portRef name = do
    openWith $ Searcher.PortName portRef def
    continue $ querySearch name

allCommands :: Items ()
allCommands = Map.fromList $ (,Element ()) . convert <$> (commands <> otherCommands) where
    commands = show <$> [(minBound :: Shortcut.Command) ..]
    otherCommands = show <$> [(minBound :: OtherCommands)]

execCommand :: Searcher -> (Event -> IO ()) -> String -> Command State ()
execCommand action scheduleEvent expression = case readMaybe expression of
    Just command -> do
        liftIO $ scheduleEvent $ Shortcut $ Shortcut.Event command def
        close action
    Nothing -> case readMaybe expression of
        Just AddNode -> modifySearcher $ do
            Searcher.selected .= def
            Searcher.mode     %= (\(Searcher.Node nl pos _) -> Searcher.Node nl pos def)
            Searcher.input    .= def
            Searcher.rollbackReady .= False
        Nothing -> return ()

acceptEntry :: (Event -> IO ()) -> Int -> Searcher -> Command State ()
acceptEntry scheduleEvent entryNum searcher = do
    mayItemsLength <- (fmap . fmap) (view Searcher.resultsLength) $ getSearcher
    withJust mayItemsLength $ \itemsLength -> when (itemsLength >= entryNum) $ do
        modifySearcher $ Searcher.selected .= entryNum
        accept scheduleEvent searcher

substituteInputWithEntry :: Searcher -> Command State ()
substituteInputWithEntry _ = do
    modifySearcher $ do
        newInput <- use Searcher.selectedExpression
        Searcher.input .= newInput
    forceSearcherInputUpdate


querySearch :: Text -> Searcher -> Command State ()
querySearch query _ = do
    selection <- Searcher.selection
    isNode <- modifySearcher $ do
        mode   <- use Searcher.mode
        isNode <- use Searcher.isNode
        Searcher.input .= query
        case mode of
            Searcher.Node _ _ _   -> Searcher.rollbackReady .= False
                {- clearing results prevents from selecting out of date result, but make searcher blink. -}
                -- Searcher.mode .= Searcher.Node def
            Searcher.NodeName _ _ -> Searcher.rollbackReady .= False
            Searcher.PortName _ _ -> Searcher.rollbackReady .= False
            Searcher.Command _ -> do
                let items = Scope.searchInScope allCommands query
                Searcher.selected      .= min 1 (length items)
                Searcher.mode          .= Searcher.Command items
                Searcher.rollbackReady .= False
        return $ All isNode
    when (getAll isNode) $ Batch.searchNodes query selection
    forceSearcherInputUpdate

forceSearcherInputUpdate :: Command State ()
forceSearcherInputUpdate = do
    modifySearcher $ Searcher.replaceInput .= True
    renderIfNeeded
    modifySearcher $ Searcher.replaceInput .= False
