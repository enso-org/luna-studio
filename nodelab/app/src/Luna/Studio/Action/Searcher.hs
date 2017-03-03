{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Action.Searcher where

import qualified Data.Map                           as Map
import           Data.Monoid                        (All (All), getAll)
import qualified Data.Text                          as Text
import           Text.Read                          (readMaybe)

import           Data.Position                      (Position)
import           Data.ScreenPosition                (ScreenPosition, x)
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as NodeAPI
import qualified Empire.API.Data.Port               as Port
import qualified JS.GoogleAnalytics                 as GA
import qualified JS.Searcher                        as Searcher
import qualified Luna.Studio.Action.Batch           as Batch
import           Luna.Studio.Action.Camera          (translateToScreen, translateToWorkspace)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Selection (selectedNodes)
import           Luna.Studio.Action.Node.Register   (registerNode)
import qualified Luna.Studio.Action.Node.Update     as Node
import qualified Luna.Studio.Batch.Workspace        as Workspace
import           Luna.Studio.Event.Event            (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut         as Shortcut
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Model.Searcher   as Searcher
import qualified Luna.Studio.React.View.App         as App
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), Searcher (Searcher), searcherAction)
import           Luna.Studio.State.Global           (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph
import           Text.ScopeSearcher.Item            (Item (..), Items)
import qualified Text.ScopeSearcher.Item            as Item
import qualified Text.ScopeSearcher.Scope           as Scope


instance Action (Command State) Searcher where
    begin    = beginActionWithKey    searcherAction
    continue = continueActionWithKey searcherAction
    update   = updateActionWithKey   searcherAction
    end      = close


data OtherCommands = AddNode
                   deriving (Bounded, Enum, Eq, Generic, Read, Show)

open :: Command State ()
open = openWith def =<< use Global.mousePos

positionDelta :: Double
positionDelta = 100

openWith :: Maybe NodeId -> ScreenPosition -> Command State ()
openWith nodeId pos = do
    pos' <- (fmap Node._position <$> selectedNodes) >>= \case
          [nodePosition] -> if isNothing nodeId then return $ nodePosition & x %~ (+positionDelta) else translateToWorkspace pos
          _              -> translateToWorkspace pos
    begin Searcher
    GA.sendEvent GA.NodeSearcher
    Global.modifyNodeEditor $ NodeEditor.searcher ?= Searcher.Searcher pos' 0 (Searcher.Node def) def nodeId
    Global.renderIfNeeded
    liftIO Searcher.focus

close :: Searcher -> Command State ()
close _ = do
    Global.modifyNodeEditor $ NodeEditor.searcher .= Nothing
    removeActionFromState searcherAction
    liftIO App.focus

moveDown :: Searcher -> Command State ()
moveDown _ = Global.modifySearcher $ do
    items <- use Searcher.resultsLength
    unless (items == 0) $
        Searcher.selected %= \p -> (p - 1) `mod` (items + 1)

moveUp :: Searcher -> Command State ()
moveUp _ = Global.modifySearcher $ do
    items <- use Searcher.resultsLength
    unless (items == 0) $
        Searcher.selected %= \p -> (p + 1) `mod` (items + 1)

proceed :: (Event -> IO ()) -> Searcher -> Command State ()
proceed scheduleEvent action = withJustM Global.getSearcher $ \searcher ->
    if searcher ^. Searcher.isNode then
        close action
    else
        accept scheduleEvent action

rollback :: Searcher -> Command State ()
rollback _ = do
    withJustM Global.getSearcher $ \searcher -> do
       when (Text.null (searcher ^. Searcher.input)
         && (searcher ^. Searcher.isNode)) $
            Global.modifySearcher $ do
                Searcher.selected .= def
                Searcher.mode     .= Searcher.Command def
                Searcher.input    .= def

accept :: (Event -> IO ()) -> Searcher -> Command State ()
accept scheduleEvent action = do
    withJustM Global.getSearcher $ \searcher -> do
        let expression = searcher ^. Searcher.selectedExpression
        if searcher ^. Searcher.isNode then do
            case searcher ^. Searcher.nodeId of
                Nothing -> registerNode (searcher ^. Searcher.position) expression
                Just nodeId-> Node.updateExpression nodeId expression
            close action
        else
            execCommand action scheduleEvent $ convert expression

openEdit :: Text -> NodeId -> Position -> Command State ()
openEdit expr nodeId pos = do
    openWith (Just nodeId) =<< translateToScreen pos
    continue $ querySearch expr

globalFunctions :: Items a -> Items a
globalFunctions = Map.filter (Item.isElement)

nodesData :: Command State (Items NodeAPI.Node)
nodesData = do
    completeData <- use $ Global.workspace . Workspace.nodeSearcherData
    selected     <- selectedNodes
    mscope <- case selected of
        [node]   -> do
            let nodeId = node ^. Node.nodeId
            mvt <- preuse $ Global.graph . Graph.nodesMap . ix nodeId . NodeAPI.ports . ix (Port.OutPortId Port.All) . Port.valueType
            return $ convert <$> mvt
        _ -> return Nothing
    case mscope of
        Nothing -> return completeData
        Just tn -> do
            let gf = globalFunctions completeData
                items = completeData
                mayScope = items ^? ix tn . Item.items
                scope = fromMaybe mempty mayScope
                scopefuns = globalFunctions scope
                overallScope = Map.union scopefuns gf
            return overallScope

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
        Just AddNode -> Global.modifySearcher $ do
            Searcher.selected .= def
            Searcher.mode     .= Searcher.Node def
            Searcher.input    .= def
        Nothing -> return ()

selectInput :: Searcher -> Command State ()
selectInput _ = Global.modifySearcher $ Searcher.selected .= 0

querySearch :: Text -> Searcher -> Command State ()
querySearch query _ = do
    selection <- Searcher.selection
    isNode <- Global.modifySearcher $ do
        isNode <- use Searcher.isNode
        Searcher.input .= query
        if isNode then
            Searcher.mode .= Searcher.Node def
        else do
            selected <- use Searcher.selected
            let items = Scope.searchInScope allCommands query
            Searcher.selected .= min 1 (length items)
            Searcher.mode .= Searcher.Command items
        return $ All isNode
    when (getAll isNode) $ Batch.nodeSearch query selection

updateHints :: Command State ()
updateHints = do
    nodesData' <- nodesData
    Global.modifySearcher $
        whenM (use Searcher.isNode) $ do
            selected <- use Searcher.selected
            query    <- use Searcher.input
            let items = Scope.searchInScope nodesData' query
            Searcher.selected .= min 1 (length items)
            Searcher.mode .= Searcher.Node items
