module Reactive.Commands.Graph.Selection
     ( selectedNodes
     , focusSelectedNode
     , selectAll
     , selectNodes
     , unselectAll
     ) where

import           Utils.PreludePlus

import           Empire.API.Data.Node         (NodeId)

import qualified React.Store                  as Store
import React.Store                  (WRef(..), ref, widget)
import qualified React.Store.Node as Node
import React.Store.Node (Node)

import           Reactive.Commands.Batch      (cancelCollaborativeTouch, collaborativeTouch)
import           Reactive.Commands.Command    (Command)
import           Reactive.Commands.Graph      (allNodes, allNodes', nodeIdToWidgetId)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.UIRegistry    as UIRegistry


unselectAll :: Command State ()
unselectAll = do
    refs <- allNodes
    nodesToCancelTouch <- forM refs $
        Store.modifyIf (view Node.isSelected) (\node ->
            ( node & Node.isSelected .~ False
            , Just $ node ^. Node.nodeId))
            (const Nothing)
    cancelCollaborativeTouch $ catMaybes nodesToCancelTouch

selectAll :: Command State ()
selectAll = do
    widgets <- allNodes'
    selectNodes $ (view $ widget . Node.nodeId) <$> widgets

selectNodes :: [NodeId] -> Command State ()
selectNodes nodeIds = do
    unselectAll
    widgetIds <- fmap catMaybes $ mapM nodeIdToWidgetId nodeIds
    inRegistry $ forM_ widgetIds $ (flip UICmd.update) (Node.isSelected .~ True)
    focusSelectedNode
    collaborativeTouch nodeIds

selectedNodes :: Command State [WRef Node]
selectedNodes = do
    widgets <- allNodes'
    return $ filter (^. widget . Node.isSelected) widgets

focusSelectedNode :: Command State ()
focusSelectedNode = do
    widgets <- selectedNodes
    inRegistry $ UIRegistry.focusedWidget .= (view ref <$> widgets ^? ix 0)
