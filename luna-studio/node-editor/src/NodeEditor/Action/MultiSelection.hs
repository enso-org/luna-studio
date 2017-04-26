{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.MultiSelection
    ( startMultiSelection
    , updateMultiSelection
    , stopMultiSelection
    ) where

import           Common.Prelude
import           Empire.API.Data.Position                   (Position, fromDoubles, x, y)
import           NodeEditor.Action.Basic                    (modifySelectionHistory, selectNodes, unselectAll)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodes, getSelectedNodes, modifyNodeEditor)
import           NodeEditor.Data.Geometry                   (isPointInRectangle)
import           NodeEditor.Event.Mouse                     (workspacePosition)
import           NodeEditor.React.Model.Node.ExpressionNode (nodeLoc, position)
import           NodeEditor.React.Model.SelectionBox        (SelectionBox (SelectionBox))
import           NodeEditor.State.Action                    (Action (begin, continue, end, update), MultiSelection (MultiSelection),
                                                             multiSelecectionStartPos, multiSelectionAction)
import           React.Flux                                 (MouseEvent)

import           NodeEditor.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                             updateActionWithKey)
import           NodeEditor.React.Model.NodeEditor          (selectionBox)
import           NodeEditor.State.Global                    (State)


instance Action (Command State) MultiSelection where
    begin    = beginActionWithKey    multiSelectionAction
    continue = continueActionWithKey multiSelectionAction
    update   = updateActionWithKey   multiSelectionAction
    end      = stopMultiSelection

startMultiSelection :: MouseEvent -> Command State ()
startMultiSelection evt = do
    unselectAll
    coord <- workspacePosition evt
    begin $ MultiSelection coord

updateMultiSelection :: MouseEvent -> MultiSelection -> Command State ()
updateMultiSelection evt state = do
    let startPos = view multiSelecectionStartPos state
    coord <- workspacePosition evt
    modifyNodeEditor $ selectionBox .= Just (SelectionBox startPos coord)
    updateSelection startPos coord

updateSelection :: Position -> Position -> Command State ()
updateSelection start act = do
    let leftTop     = fromDoubles (min (start ^. x) (act ^. x)) (min (start ^. y) (act ^. y))
        rightBottom = fromDoubles (max (start ^. x) (act ^. x)) (max (start ^. y) (act ^. y))
    nodeLocs <- map (view nodeLoc) . filter (flip isPointInRectangle (leftTop, rightBottom) . (view position)) <$> getExpressionNodes
    selectNodes nodeLocs

stopMultiSelection :: MultiSelection -> Command State ()
stopMultiSelection _ = do
    removeActionFromState multiSelectionAction
    modifyNodeEditor $ selectionBox .= Nothing
    nodeLocs <- map (view nodeLoc) <$> getSelectedNodes
    modifySelectionHistory nodeLocs
