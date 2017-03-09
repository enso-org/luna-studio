{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Action.Graph.NodeSearch where

import qualified Data.Map                           as Map
import           Data.Monoid                        (All (All), getAll)
import           Data.Position                      (Position)
import           Data.ScreenPosition                (ScreenPosition, x)
import qualified Data.Text                          as Text
import           Empire.API.Data.Node               (Node, NodeId)
import qualified Empire.API.Data.Node               as Node
import qualified Empire.API.Data.Port               as Port
import qualified JS.GoogleAnalytics                 as GA
import qualified JS.Searcher                        as Searcher
import qualified Luna.Studio.Action.Batch           as Batch
import           Luna.Studio.Action.Camera          (translateToScreen, translateToWorkspace)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Selection (selectedNodes)
import           Text.Read                          (readMaybe)
-- import           Luna.Studio.Action.Node.Register   (registerNode)
import           Luna.Studio.Action.Graph.AddNode   (addNode)
import qualified Luna.Studio.Action.Node.Update     as Node
import qualified Luna.Studio.Batch.Workspace        as Workspace
import           Luna.Studio.Event.Event            (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut         as Shortcut
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Model
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


globalFunctions :: Items a -> Items a
globalFunctions = Map.filter (Item.isElement)

nodesData :: Command State (Items Node)
nodesData = do
    completeData <- use $ Global.workspace . Workspace.nodeSearcherData
    selected     <- selectedNodes
    mscope <- case selected of
        [node]   -> do
            let nodeId = node ^. Model.nodeId
            mvt <- preuse $ Global.graph . Graph.nodesMap . ix nodeId . Node.ports . ix (Port.OutPortId Port.All) . Port.valueType
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

updateHints :: Items Node -> Command State ()
updateHints items = do
    Global.workspace . Workspace.nodeSearcherData .= items
    nodesData' <- nodesData
    Global.modifySearcher $
        whenM (use Searcher.isNode) $ do
            query    <- use Searcher.input
            let items' = Scope.searchInScope nodesData' query
            Searcher.selected      .= min 1 (length items')
            Searcher.rollbackReady .= False
            Searcher.mode          .= Searcher.Node items'
