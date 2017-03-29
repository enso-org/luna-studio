--TODO[LJK, PM]: Refactor
module Luna.Studio.Action.Basic.SelectNode where

import qualified Data.HashMap.Strict                         as Map
import qualified Data.Set                                    as Set
import           Luna.Studio.Action.Basic.FocusNode          (focusNodes)
import           Luna.Studio.Action.Batch                    (cancelCollaborativeTouch, collaborativeTouch)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNode, getExpressionNodes, getSelectedNodes, modifyExpressionNode,
                                                              modifyNodeEditor)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeLoc, isSelected, nodeLoc)
import           Luna.Studio.React.Model.NodeEditor          (expressionNodes)
import           Luna.Studio.State.Global                    (State, selectionHistory)


selectAll :: Command State ()
selectAll = do
    nodeLocs <- map (view nodeLoc) <$> getExpressionNodes
    forM_ nodeLocs $ \nl -> modifyExpressionNode nl $ isSelected .= True
    focusNodes nodeLocs
    collaborativeTouch nodeLocs
    modifySelectionHistory nodeLocs

selectNode :: NodeLoc -> Command State ()
selectNode = selectNodes . return

selectNodes :: [NodeLoc] -> Command State ()
selectNodes nodeLocs = do
    selected <- map (view nodeLoc) <$> getSelectedNodes
    let nodesToSelect   = nodeLocs \\ selected
        nodesToUnselect = selected \\ nodeLocs
    addToSelection nodesToSelect
    removeFromSelection nodesToUnselect
    modifySelectionHistory nodeLocs

selectPreviousNodes :: Command State ()
selectPreviousNodes = do
    maybeSelection <- listToMaybe <$> use selectionHistory
    flip (maybe dropSelectionHistory) maybeSelection $ \nodeLocs -> do
        selectionHistory %= drop 1
        unselectAll
        addToSelection $ Set.toList nodeLocs
        map (view nodeLoc) <$> getSelectedNodes >>= \selection ->
            if null selection
                then selectPreviousNodes
                else modifySelectionHistory selection

unselectAll :: Command State ()
unselectAll = do
    prevSelected <- map (view nodeLoc) <$> getSelectedNodes
    forM_ prevSelected $ \prev ->
        modifyExpressionNode prev $ isSelected .= False
    cancelCollaborativeTouch prevSelected

toggleSelect :: NodeLoc -> Command State ()
toggleSelect nl = getExpressionNode nl >>= \mayNode ->
    withJust mayNode $ \node ->
        if node ^. isSelected
            then do
                modifyExpressionNode nl $ isSelected .= False
                selection <- map (view nodeLoc) <$> getSelectedNodes
                if null selection
                    then dropSelectionHistory
                    else modifySelectionHistory selection
            else addToSelection [nl]



-- Please be aware that this function modifies selection without changing selection history.
-- If your new selection should be added to history launch modifySelectionHistory with ids from result.
addToSelection :: [NodeLoc] -> Command State ()
addToSelection nodeLocs = do
    forM_ nodeLocs $ \nl -> modifyExpressionNode nl $ isSelected .= True
    focusNodes nodeLocs
    collaborativeTouch nodeLocs

-- Please be aware that this function modifies selection without changing selection history.
-- If your new selection should be added to history launch modifySelectionHistory with ids from result.
removeFromSelection :: [NodeLoc] -> Command State ()
removeFromSelection nodeLocs = do
    forM_ nodeLocs $ \nl -> modifyExpressionNode nl $ isSelected .= False
    cancelCollaborativeTouch nodeLocs

historyMaxLength :: Int
historyMaxLength = 10

dropSelectionHistory :: Command State ()
dropSelectionHistory = selectionHistory .= def

modifySelectionHistory :: [NodeLoc] -> Command State ()
modifySelectionHistory nodeLocs = do
    maybeSelectionSet <- uses selectionHistory listToMaybe
    let nodeLocsSet = Set.fromList nodeLocs
    case maybeSelectionSet of
        Nothing        -> selectionHistory .= [nodeLocsSet]
        Just selection -> when (nodeLocsSet /= selection) $
            selectionHistory %= take historyMaxLength . (nodeLocsSet :)
    when (Set.null nodeLocsSet) dropSelectionHistory

unselectAllAndDropSelectionHistory :: Command State ()
unselectAllAndDropSelectionHistory = unselectAll >> dropSelectionHistory
