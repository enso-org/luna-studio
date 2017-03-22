{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Visualization
    ( pin
    , unpin
    , startDrag
    , stopDrag
    , drag
    ) where

import           React.Flux                                  (MouseEvent)

import           Data.Position                               (Position)
import           Empire.API.Data.Node                        (NodeId)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                              updateActionWithKey)
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNode, modifyExpressionNodeEditor)
import           Luna.Studio.Event.Mouse                     (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (position)
import           Luna.Studio.React.Model.NodeEditor          (visualizations)
import           Luna.Studio.State.Action                    (Action (begin, continue, end, update), VisualizationDrag (VisualizationDrag),
                                                              visualizationDragAction)
import           Luna.Studio.State.Global                    (State)


instance Action (Command State) VisualizationDrag where
    begin    = beginActionWithKey    visualizationDragAction
    continue = continueActionWithKey visualizationDragAction
    update   = updateActionWithKey   visualizationDragAction
    end _    = removeActionFromState visualizationDragAction


pin :: NodeId -> Int -> Command State ()
pin nid visIx = do
    mayNode <- getExpressionNode nid
    withJust mayNode $ \node ->
        modifyExpressionNodeEditor $
            visualizations %= ((nid, visIx, node ^. position) :)

unpin :: NodeId -> Int -> Position -> Command State ()
unpin nid visIx pos =
    modifyExpressionNodeEditor $ visualizations %= delete (nid, visIx, pos)

startDrag :: NodeId -> Int -> Position -> MouseEvent -> Command State ()
startDrag nid visIx pos evt = do
    begin $ VisualizationDrag nid visIx pos
    moveTo evt nid visIx pos

drag :: MouseEvent -> VisualizationDrag -> Command State ()
drag evt (VisualizationDrag nid visIx pos) = moveTo evt nid visIx pos

stopDrag :: MouseEvent -> VisualizationDrag ->  Command State ()
stopDrag evt (VisualizationDrag nid visIx pos) = do
    moveTo evt nid visIx pos
    removeActionFromState visualizationDragAction

moveTo :: MouseEvent -> NodeId -> Int -> Position -> Command State ()
moveTo evt nid visIx oldPos = do
    pos <- workspacePosition evt
    update $ VisualizationDrag nid visIx pos
    modifyExpressionNodeEditor $ do
        visualizations %= delete (nid, visIx, oldPos)
        visualizations %= ((nid, visIx, pos) :)
