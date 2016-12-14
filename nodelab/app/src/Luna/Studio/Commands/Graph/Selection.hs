module Luna.Studio.Commands.Graph.Selection
     ( addToSelection
     , dropSelectionHistory
     , focusSelectedNode
     , modifySelectionHistory
     , selectAll
     , selectedNodes
     , selectNodes
     , selectPreviousNodes
     , toggleSelect
     , unselectAll
     , unselectAllAndDropSelectionHistory
     ) where

import qualified Data.Set                                 as Set
import           Luna.Studio.Prelude

import           Empire.API.Data.Node                     (NodeId)

import           Luna.Studio.React.Store                              (WRef (..), widget)
import qualified Luna.Studio.React.Store                              as Store
import           Luna.Studio.React.Model.Node                         (Node)
import qualified Luna.Studio.React.Model.Node                         as Node

import           Luna.Studio.Commands.Batch                  (cancelCollaborativeTouch, collaborativeTouch)
import           Luna.Studio.Commands.Command                (Command)
import           Luna.Studio.Commands.Graph                  (allNodes, allNodes')
import           Luna.Studio.Commands.Graph.SelectionHistory
import           Luna.Studio.State.Global                    (State, inRegistry)
import qualified Luna.Studio.State.Global                    as Global
import qualified Luna.Studio.State.UIRegistry                as UIRegistry




toggleSelect :: NodeId -> Command Global.State ()
toggleSelect nodeId = do
    mayNodeRef <- Global.getNode nodeId
    withJust mayNodeRef $ \nodeRef -> do
        isSelected <- view Node.isSelected <$> Store.get nodeRef
        if isSelected
            then do
                Store.modify_ (Node.isSelected .~ False) nodeRef
                selection <- map (view $ widget . Node.nodeId) <$> selectedNodes
                case selection of
                    [] -> dropSelectionHistory
                    _  -> modifySelectionHistory selection
            else addToSelection [nodeId] >> return ()

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
selectNodes nodeIds = unselectAll >> addToSelection nodeIds >>= modifySelectionHistory

-- Please be aware that this function modifies selection without changing selection history.
-- If your new selection should be added to history launch modifySelectionHistory with ids from result.
addToSelection :: [NodeId] -> Command State [NodeId]
addToSelection nodeIds = do
    nodeRefs <- fmap catMaybes $ mapM Global.getNode nodeIds
    forM_ nodeRefs $ Store.modify_ (Node.isSelected .~ True)
    focusSelectedNode
    collaborativeTouch nodeIds
    map (^. widget . Node.nodeId) <$> selectedNodes

selectPreviousNodes :: Command State ()
selectPreviousNodes = do
    maybeSelection <- uses Global.selectionHistory listToMaybe
    case maybeSelection of
        Nothing         -> dropSelectionHistory
        Just nodeIdsSet -> do
            Global.selectionHistory %= drop 1
            unselectAll
            selection <- addToSelection $ Set.toList nodeIdsSet
            case selection of
                [] -> selectPreviousNodes
                _  -> modifySelectionHistory selection

selectedNodes :: Command State [WRef Node]
selectedNodes = do
    widgets <- allNodes'
    return $ filter (^. widget . Node.isSelected) widgets

focusSelectedNode :: Command State ()
focusSelectedNode = do
    return () --TODO[react]
    -- widgets <- selectedNodes
    -- inRegistry $ UIRegistry.focusedWidget .= (view ref <$> widgets ^? ix 0)
