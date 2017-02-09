{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Action.Searcher where

import qualified Data.Map                           as Map
import qualified Data.Text                          as Text

import           Data.Position                      (Position)
import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.Node               as NodeAPI
import qualified Empire.API.Data.Port               as Port
import qualified Empire.API.Data.TypeRep            as TypeRep
import qualified Empire.API.Data.ValueType          as ValueType
import qualified JS.GoogleAnalytics                 as GA
import           Luna.Studio.Action.Camera          (translateToWorkspace)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Selection (selectedNodes)
import           Luna.Studio.Action.Node.Register   (registerNode)
import qualified Luna.Studio.Action.Node.Update     as Node
import qualified Luna.Studio.Batch.Workspace        as Workspace
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.App        as App
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.Searcher   as Searcher
import qualified Luna.Studio.React.View.App         as App
import qualified Luna.Studio.React.View.Searcher    as Searcher
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), Searcher (Searcher), searcherAction)
import           Luna.Studio.State.Global           (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph
import           Text.ScopeSearcher.Item            (Item (..), Items, _Group)
import qualified Text.ScopeSearcher.Scope           as Scope


instance Action (Command State) Searcher where
    begin    = beginActionWithKey    searcherAction
    continue = continueActionWithKey searcherAction
    update   = updateActionWithKey   searcherAction
    end      = close


searcherData :: Command State Items
searcherData = use $ Global.workspace . Workspace.nodeSearcherData

open :: Command State ()
open = openWith def =<< use Global.mousePos

openWith :: Maybe NodeId -> Position -> Command State ()
openWith nodeId pos = do
    begin Searcher
    GA.sendEvent GA.NodeSearcher
    Global.modifyApp $ App.searcher ?= Searcher.Searcher pos 0 (Searcher.Node def) def def nodeId
    Global.renderIfNeeded
    liftIO Searcher.focus

close :: Searcher -> Command State ()
close _ = do
    Global.modifyApp $ App.searcher .= Nothing
    removeActionFromState searcherAction
    liftIO App.focus

moveDown :: Searcher -> Command State ()
moveDown _ = Global.modifySearcher $ do
    items <- length <$> use Searcher.results
    unless (items == 0) $
        Searcher.selected %= \p -> (p + 1) `mod` items

moveUp :: Searcher -> Command State ()
moveUp _ = Global.modifySearcher $ do
    items <- length <$> use Searcher.results
    unless (items == 0) $
        Searcher.selected %= \p -> (p - 1) `mod` items

proceed :: Searcher -> Command State ()
proceed _ = do
    withJustM Global.getSearcher $ \searcher ->
        case searcher ^. Searcher.context of
            -- Searcher.Command   -> parseCommand
            Searcher.Node prev -> Global.modifySearcher $ do
                let expression = searcher ^. Searcher.selectedExpression
                Searcher.context  .= Searcher.Node (expression : prev)
                Searcher.input    .= def
                Searcher.selected .= def

rollback :: Searcher -> Command State ()
rollback _ = do
    withJustM Global.getSearcher $ \searcher -> do
        case searcher ^. Searcher.context of
            Searcher.Command -> return ()
            Searcher.Node prev -> Global.modifySearcher $ do
                if Text.null $ searcher ^. Searcher.input then do
                    Searcher.selected .= def
                    case prev of
                        [] -> do
                            Searcher.context  .= Searcher.Command
                            Searcher.input    .= def
                        (h:t) -> do
                            Searcher.context  .= Searcher.Node t
                            Searcher.input    .= h
                else
                    Searcher.input %= Text.init

accept :: Searcher -> Command State ()
accept action = do
    withJustM Global.getSearcher $ \searcher -> do
        pos <- translateToWorkspace (searcher ^. Searcher.position)
        let expression = searcher ^. Searcher.selectedExpression
        case searcher ^. Searcher.nodeId of
            Nothing -> registerNode pos expression
            Just nodeId-> Node.updateExpression nodeId expression
    close action

openEdit :: Text -> NodeId -> Position -> Command State ()
openEdit expr nodeId pos = do
    openWith (Just nodeId) pos
    continue $ querySearch expr

globalFunctions :: Items -> Items
globalFunctions = Map.filter (== Element)

scopedData :: Command State Items
scopedData = do
    completeData <- searcherData
    selected   <- selectedNodes
    mscope <- case selected of
        [node]   -> do
            let nodeId = node ^. Node.nodeId
            mvt <- preuse $ Global.graph . Graph.nodesMap . ix nodeId . NodeAPI.ports . ix (Port.OutPortId Port.All) . Port.valueType
            return $ case mvt of
                Nothing -> Nothing
                Just vt -> case vt of
                    ValueType.TypeIdent (TypeRep.TCons ti _) -> Just $ convert ti
                    _ -> Nothing
        _ -> return Nothing
    case mscope of
        Nothing -> return completeData
        Just tn -> do
            let gf = globalFunctions completeData
                items = completeData
                mayScope = items ^? ix tn . _Group
                scope = fromMaybe mempty mayScope
                scopefuns = globalFunctions scope
                overallScope = Map.union scopefuns gf
            return overallScope


querySearch :: Text -> Searcher -> Command State ()
querySearch query _ = do
    sd <- scopedData
    let items = Scope.searchInScope sd query
    Global.modifySearcher $ do
        Searcher.input .= query
        s <- use Searcher.selected
        when (s >= length items) $
            Searcher.selected .= length items - 1
        Searcher.results .= items
