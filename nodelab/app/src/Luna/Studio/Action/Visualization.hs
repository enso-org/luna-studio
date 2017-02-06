{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Visualization
( pin
, unpin
, startDrag
, stopDrag
, drag
)where

import           React.Flux                         (MouseEvent)

import           Data.Position                      (Position)
import           Empire.API.Data.Node               (NodeId)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Event.Mouse            (workspacePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), VisualizationDrag (VisualizationDrag),
                                                     visualizationDragAction)
import           Luna.Studio.State.Global           (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import qualified Luna.Studio.State.Global           as Global


pin :: NodeId -> Int -> Command State ()
pin nodeId visIx = do
    mayNode <- Global.getNode nodeId
    withJust mayNode $ \node ->
        Global.modifyNodeEditor $
            NodeEditor.visualizations %= ((nodeId, visIx, node ^. Node.position) :)

unpin :: NodeId -> Int -> Position -> Command State ()
unpin nodeId visIx position =
    Global.modifyNodeEditor $
        NodeEditor.visualizations %= delete (nodeId, visIx, position)

instance Action (Command State) VisualizationDrag where
    begin    = beginActionWithKey    visualizationDragAction
    continue = continueActionWithKey visualizationDragAction
    update   = updateActionWithKey   visualizationDragAction
    end _    = removeActionFromState visualizationDragAction

startDrag :: NodeId -> Int -> Position -> MouseEvent -> Command State ()
startDrag nodeId visIx position evt = do
    begin $ VisualizationDrag nodeId visIx position
    moveTo evt nodeId visIx position

drag :: MouseEvent -> VisualizationDrag -> Command State ()
drag evt (VisualizationDrag nodeId visIx position) =
    moveTo evt nodeId visIx position

stopDrag :: MouseEvent -> VisualizationDrag ->  Command State ()
stopDrag evt (VisualizationDrag nodeId visIx position) = do
    moveTo evt nodeId visIx position
    removeActionFromState visualizationDragAction

moveTo :: MouseEvent -> NodeId -> Int -> Position -> Command State ()
moveTo evt nodeId visIx oldPos = do
    position <- workspacePosition evt
    update $ VisualizationDrag nodeId visIx position
    Global.modifyNodeEditor $ do
        NodeEditor.visualizations %= delete (nodeId, visIx, oldPos)
        NodeEditor.visualizations %= ((nodeId, visIx, position) :)
