module Reactive.Commands.Graph.Selection
     ( selectedNodes
     , focusSelectedNode
     , selectAll
     , selectNodes
     , unselectAll
     , unselectAllAndDropSelectionHistory
     , dropSelectionHistory
     , modifySelectionHistory
     , selectPreviousNodes
     ) where

import qualified Data.Set                                 as Set
import           Utils.PreludePlus

import           Empire.API.Data.Node                     (NodeId)

import           React.Store                              (WRef (..), widget)
import qualified React.Store                              as Store
import           React.Store.Node                         (Node)
import qualified React.Store.Node                         as Node

import           Reactive.Commands.Batch                  (cancelCollaborativeTouch, collaborativeTouch)
import           Reactive.Commands.Command                (Command)
import           Reactive.Commands.Graph                  (allNodes, allNodes')
import           Reactive.Commands.Graph.SelectionHistory
import           Reactive.State.Global                    (State, inRegistry)
import qualified Reactive.State.Global                    as Global
import qualified Reactive.State.UIRegistry                as UIRegistry



unselectAll :: Command State ()
unselectAll = do
    refs <- allNodes
    nodesToCancelTouch <- forM refs $
        Store.modifyIf (view Node.isSelected) (\node ->
            ( node & Node.isSelected .~ False
            , Just $ node ^. Node.nodeId))
            (const Nothing)
    inRegistry $ UIRegistry.focusedWidget .= def
    cancelCollaborativeTouch $ catMaybes nodesToCancelTouch

unselectAllAndDropSelectionHistory :: Command State ()
unselectAllAndDropSelectionHistory = unselectAll >> dropSelectionHistory

selectAll :: Command State ()
selectAll = do
    widgets <- allNodes'
    selectNodes $ (view $ widget . Node.nodeId) <$> widgets

selectNodes :: [NodeId] -> Command State ()
selectNodes nodeIds = selectNodes' nodeIds >> modifySelectionHistory nodeIds

selectNodes' :: [NodeId] -> Command State ()
selectNodes' nodeIds = do
    unselectAll
    nodeRefs <- fmap catMaybes $ mapM Global.getNode nodeIds
    forM_ nodeRefs $ Store.modify_ (Node.isSelected .~ True)
    focusSelectedNode
    collaborativeTouch nodeIds

selectPreviousNodes :: Command State ()
selectPreviousNodes = do
    maybeSelection <- uses Global.selectionHistory listToMaybe
    case maybeSelection of
        Nothing         -> dropSelectionHistory
        Just nodeIdsSet -> do
            Global.selectionHistory %= drop 1
            selectNodes' $ Set.toList nodeIdsSet
            selection <- map (^. widget . Node.nodeId) <$> selectedNodes
            case selection of
                []        -> selectPreviousNodes
                otherwise -> modifySelectionHistory selection

selectedNodes :: Command State [WRef Node]
selectedNodes = do
    widgets <- allNodes'
    return $ filter (^. widget . Node.isSelected) widgets

focusSelectedNode :: Command State ()
focusSelectedNode = do
    return () --TODO[react]
    -- widgets <- selectedNodes
    -- inRegistry $ UIRegistry.focusedWidget .= (view ref <$> widgets ^? ix 0)
