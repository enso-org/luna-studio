module Luna.Studio.Action.Visualization
( pin
, unpin
, startDrag
, stopDrag
, drag
)where

import           React.Flux                         (MouseEvent)

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
            NodeEditor.visualizations . at (nodeId, visIx) ?= node ^. Node.position

unpin :: NodeId -> Int -> Command State ()
unpin nodeId visIx =
    Global.modifyNodeEditor $
        NodeEditor.visualizations . at (nodeId, visIx) .= def

instance Action (Command State) VisualizationDrag where
    begin    = beginActionWithKey    visualizationDragAction
    continue = continueActionWithKey visualizationDragAction
    update   = updateActionWithKey   visualizationDragAction
    end _    = removeActionFromState visualizationDragAction

startDrag :: NodeId -> Int -> MouseEvent -> Command State ()
startDrag nodeId visIx evt = do
    begin $ VisualizationDrag nodeId visIx
    moveTo evt nodeId visIx

drag :: MouseEvent -> VisualizationDrag -> Command State ()
drag evt (VisualizationDrag nodeId visIx) =
    moveTo evt nodeId visIx

stopDrag :: MouseEvent -> VisualizationDrag ->  Command State ()
stopDrag evt (VisualizationDrag nodeId visIx) = do
    removeActionFromState visualizationDragAction
    moveTo evt nodeId visIx

moveTo :: MouseEvent -> NodeId -> Int -> Command State ()
moveTo evt nodeId visIx = do
    position <- workspacePosition evt
    Global.modifyNodeEditor $
        NodeEditor.visualizations . at (nodeId, visIx) ?= position
