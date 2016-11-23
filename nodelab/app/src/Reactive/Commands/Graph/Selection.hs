module Reactive.Commands.Graph.Selection
     ( selectedNodes
     , focusSelectedNode
     , selectAll
     , selectNodes
     , unselectAll
     ) where

import           Utils.PreludePlus

import           Empire.API.Data.Node         (NodeId)

import           Object.Widget                (WidgetFile (..), objectId, widget)
import qualified Object.Widget.Node           as NodeModel

import           Reactive.Commands.Batch      (cancelCollaborativeTouch, collaborativeTouch)
import           Reactive.Commands.Command    (Command)
import           Reactive.Commands.Graph      (allNodes, nodeIdToWidgetId)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.UIRegistry    as UIRegistry


unselectAll :: Command State ()
unselectAll = do
    widgets <- allNodes
    nodesToCancelTouch <- inRegistry $ forM widgets $ \wf -> do
        let widgetId = wf ^. objectId
        if (wf ^. widget . NodeModel.isSelected) then do
                UICmd.update_ widgetId $ NodeModel.isSelected .~ False
                return $ Just $ wf ^. widget . NodeModel.nodeId
        else return Nothing

    cancelCollaborativeTouch $ catMaybes nodesToCancelTouch

selectAll :: Command State ()
selectAll = do
    widgets <- allNodes
    selectNodes $ (view $ widget . NodeModel.nodeId) <$> widgets

selectNodes :: [NodeId] -> Command State ()
selectNodes nodeIds = do
    unselectAll
    widgetIds <- fmap catMaybes $ mapM nodeIdToWidgetId nodeIds
    inRegistry $ forM_ widgetIds $ (flip UICmd.update) (NodeModel.isSelected .~ True)
    focusSelectedNode
    collaborativeTouch nodeIds

selectedNodes :: Command State [WidgetFile NodeModel.Node]
selectedNodes = do
    widgets <- allNodes
    return $ filter (^. widget . NodeModel.isSelected) widgets

focusSelectedNode :: Command State ()
focusSelectedNode = do
    widgets <- selectedNodes
    inRegistry $ UIRegistry.focusedWidget .= (view objectId <$> widgets ^? ix 0)
