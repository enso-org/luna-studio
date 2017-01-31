module Luna.Studio.Action.Graph.Selection
     ( addToSelection
     , dropSelectionHistory
     , modifySelectionHistory
     , selectAll
     , selectedNodes
     , selectedNodeIds
     , selectNodes
     , selectPreviousNodes
     , toggleSelect
     , unselectAll
     , unselectAllAndDropSelectionHistory
     ) where

import qualified Data.HashMap.Strict                       as HashMap
import qualified Data.Set                                  as Set

import           Empire.API.Data.Node                      (NodeId)
import           Luna.Studio.Action.Batch                  (cancelCollaborativeTouch, collaborativeTouch)
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Action.Graph.Focus            (focusNodes)
import           Luna.Studio.Action.Graph.Lookup           (allNodeIds, allNodes)
import           Luna.Studio.Action.Graph.SelectionHistory (dropSelectionHistory, modifySelectionHistory)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node              (Node)
import qualified Luna.Studio.React.Model.Node              as Node
import qualified Luna.Studio.React.Model.NodeEditor        as NodeEditor
import           Luna.Studio.State.Global                  (State)
import qualified Luna.Studio.State.Global                  as Global



toggleSelect :: NodeId -> Command Global.State ()
toggleSelect nodeId = do
    mayNode <- Global.getNode nodeId
    withJust mayNode $ \node -> do
        if node ^. Node.isSelected
            then do
                Global.modifyNode nodeId $ Node.isSelected .= False
                selection <- selectedNodeIds
                case selection of
                    [] -> dropSelectionHistory
                    _  -> modifySelectionHistory selection
            else addToSelection [nodeId] >> return ()

unselectAll :: Command State ()
unselectAll = do
    nodesToCancelTouch <- selectedNodeIds
    Global.modifyNodeEditor $
        NodeEditor.nodes %= HashMap.map (Node.isSelected .~ False)
    cancelCollaborativeTouch nodesToCancelTouch

unselectAllAndDropSelectionHistory :: Command State ()
unselectAllAndDropSelectionHistory = unselectAll >> dropSelectionHistory

selectAll :: Command State ()
selectAll = allNodeIds >>= selectNodes

selectNodes :: [NodeId] -> Command State ()
selectNodes nodeIds = do
    alreadySelected <- selectedNodeIds
    let nodesToSelect = nodeIds \\ alreadySelected
        nodesToUnselect = alreadySelected \\ nodeIds
    addToSelection nodesToSelect
    removeFromSelection nodesToUnselect
    modifySelectionHistory nodeIds

-- Please be aware that this function modifies selection without changing selection history.
-- If your new selection should be added to history launch modifySelectionHistory with ids from result.
addToSelection :: [NodeId] -> Command State ()
addToSelection nodeIds = do
    Global.modifyNodeEditor $ forM_ nodeIds $ \nodeId ->
        NodeEditor.nodes . at nodeId %= fmap (Node.isSelected .~ True)
    focusNodes nodeIds
    collaborativeTouch nodeIds

-- Please be aware that this function modifies selection without changing selection history.
-- If your new selection should be added to history launch modifySelectionHistory with ids from result.
removeFromSelection :: [NodeId] -> Command State ()
removeFromSelection nodeIds = do
    Global.modifyNodeEditor $ forM_ nodeIds $ \nodeId ->
        NodeEditor.nodes . at nodeId %= fmap (Node.isSelected .~ False)
    cancelCollaborativeTouch nodeIds

selectPreviousNodes :: Command State ()
selectPreviousNodes = do
    maybeSelection <- uses Global.selectionHistory listToMaybe
    case maybeSelection of
        Nothing         -> dropSelectionHistory
        Just nodeIdsSet -> do
            Global.selectionHistory %= drop 1
            unselectAll
            addToSelection $ Set.toList nodeIdsSet
            selectedNodeIds >>= \case
                []        -> selectPreviousNodes
                selection -> modifySelectionHistory selection

selectedNodes :: Command State [Node]
selectedNodes = do
    nodes <- allNodes
    return $ filter (^. Node.isSelected) nodes

selectedNodeIds :: Command State [NodeId]
selectedNodeIds = map (view Node.nodeId) <$> selectedNodes
