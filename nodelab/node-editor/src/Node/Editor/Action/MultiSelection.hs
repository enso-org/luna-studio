{-# OPTIONS_GHC -fno-warn-orphans #-}
module Node.Editor.Action.MultiSelection
    ( startMultiSelection
    , updateMultiSelection
    , stopMultiSelection
    ) where

import           Data.Position                               (Position, fromDoubles, x, y)
import           Node.Editor.Action.Basic                    (modifySelectionHistory, selectNodes, unselectAll)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.State.NodeEditor         (getExpressionNodes, getSelectedNodes, modifyNodeEditor)
import           Node.Editor.Data.Geometry                   (isPointInRectangle)
import           Node.Editor.Event.Mouse                     (workspacePosition)
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (nodeLoc, position)
import           Node.Editor.React.Model.SelectionBox        (SelectionBox (SelectionBox))
import           Node.Editor.State.Action                    (Action (begin, continue, end, update), MultiSelection (MultiSelection),
                                                              multiSelecectionStartPos, multiSelectionAction)
import           React.Flux                                  (MouseEvent)

import           Node.Editor.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                              updateActionWithKey)
import           Node.Editor.React.Model.NodeEditor          (selectionBox)
import           Node.Editor.State.Global                    (State)


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
