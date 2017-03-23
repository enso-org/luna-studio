--TODO[LJK, PM]: Refactor
module Luna.Studio.Action.Basic.SelectNode where

import qualified Data.HashMap.Strict                         as Map
import qualified Data.Set                                    as Set
import           Empire.API.Data.Node                        (NodeId)
import           Luna.Studio.Action.Basic.FocusNode          (focusNodes)
import           Luna.Studio.Action.Batch                    (cancelCollaborativeTouch, collaborativeTouch)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNode, getExpressionNodes, getSelectedNodes, modifyExpressionNode,
                                                              modifyNodeEditor)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (isSelected, nodeId)
import           Luna.Studio.React.Model.NodeEditor          (expressionNodes)
import           Luna.Studio.State.Global                    (State, selectionHistory)


selectAll :: Command State ()
selectAll = do
    nodeIds <- map (view nodeId) <$> getExpressionNodes
    forM_ nodeIds $ \nid -> modifyExpressionNode nid $ isSelected .= True
    focusNodes nodeIds
    collaborativeTouch nodeIds
    modifySelectionHistory nodeIds

selectNode :: NodeId -> Command State ()
selectNode = selectNodes . return

selectNodes :: [NodeId] -> Command State ()
selectNodes nodeIds = do
    selected <- map (view nodeId) <$> getSelectedNodes
    let nodesToSelect   = nodeIds \\ selected
        nodesToUnselect = selected \\ nodeIds
    addToSelection nodesToSelect
    removeFromSelection nodesToUnselect
    modifySelectionHistory nodeIds

selectPreviousNodes :: Command State ()
selectPreviousNodes = do
    maybeSelection <- listToMaybe <$> use selectionHistory
    flip (maybe dropSelectionHistory) maybeSelection $ \nodeIds -> do
        selectionHistory %= drop 1
        unselectAll
        addToSelection $ Set.toList nodeIds
        map (view nodeId) <$> getSelectedNodes >>= \selection ->
            if null selection
                then selectPreviousNodes
                else modifySelectionHistory selection

unselectAll :: Command State ()
unselectAll = do
    prevSelected <- map (view nodeId) <$> getSelectedNodes
    modifyNodeEditor $ expressionNodes %= Map.map (isSelected .~ False)
    cancelCollaborativeTouch prevSelected

toggleSelect :: NodeId -> Command State ()
toggleSelect nid = getExpressionNode nid >>= \mayNode ->
    withJust mayNode $ \node ->
        if node ^. isSelected
            then do
                modifyExpressionNode nid $ isSelected .= False
                selection <- map (view nodeId) <$> getSelectedNodes
                if null selection
                    then dropSelectionHistory
                    else modifySelectionHistory selection
            else addToSelection [nid]



-- Please be aware that this function modifies selection without changing selection history.
-- If your new selection should be added to history launch modifySelectionHistory with ids from result.
addToSelection :: [NodeId] -> Command State ()
addToSelection nodeIds = do
    forM_ nodeIds $ \nid -> modifyExpressionNode nid $ isSelected .= True
    focusNodes nodeIds
    collaborativeTouch nodeIds

-- Please be aware that this function modifies selection without changing selection history.
-- If your new selection should be added to history launch modifySelectionHistory with ids from result.
removeFromSelection :: [NodeId] -> Command State ()
removeFromSelection nodeIds = do
    forM_ nodeIds $ \nid -> modifyExpressionNode nid $ isSelected .= False
    cancelCollaborativeTouch nodeIds

historyMaxLength :: Int
historyMaxLength = 10

dropSelectionHistory :: Command State ()
dropSelectionHistory = selectionHistory .= def

modifySelectionHistory :: [NodeId] -> Command State ()
modifySelectionHistory nodeIds = do
    maybeSelectionSet <- uses selectionHistory listToMaybe
    let nodeIdsSet = Set.fromList nodeIds
    case maybeSelectionSet of
        Nothing        -> selectionHistory .= [nodeIdsSet]
        Just selection -> when (nodeIdsSet /= selection) $
            selectionHistory %= take historyMaxLength . (nodeIdsSet :)
    when (Set.null nodeIdsSet) dropSelectionHistory

unselectAllAndDropSelectionHistory :: Command State ()
unselectAllAndDropSelectionHistory = unselectAll >> dropSelectionHistory
