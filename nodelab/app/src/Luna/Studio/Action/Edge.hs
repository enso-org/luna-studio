{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Edge
    ( handleMove
    , restoreConnect
    , removePort
    , addPort
    , endPortDrag
    ) where

import           Empire.API.Data.Node               (NodeId)
import qualified Luna.Studio.Action.Batch           as Batch
import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.Action.Connect         as Connect
import           Luna.Studio.Action.Graph.Lookup    (getPort)
import           Luna.Studio.Event.Mouse            (workspacePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.React.Model.Port       (DraggedPort (DraggedPort))
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), Connect, PortDrag (PortDrag), portDragAction)
import qualified Luna.Studio.State.Action           as Action
import           Luna.Studio.State.Global           (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import qualified Luna.Studio.State.Global           as Global
import           React.Flux                         (MouseEvent)


instance Action (Command State) PortDrag where
    begin    = beginActionWithKey    portDragAction
    continue = continueActionWithKey portDragAction
    update   = updateActionWithKey   portDragAction
    end      = endPortDrag

handleMove :: MouseEvent -> Command State ()
handleMove evt = do
    continue $ connectOrPortDrag evt
    continue $ moveDraggedPort   evt

connectOrPortDrag :: MouseEvent -> Connect -> Command State ()
connectOrPortDrag evt connect = if (connect ^. Action.connectIsConnModified) then
         continue $ Connect.handleMove evt
    else begin $ PortDrag (connect ^. Action.connectStartPos) (connect ^. Action.connectSourcePort) (connect ^. Action.connectMode)

moveDraggedPort :: MouseEvent -> PortDrag -> Command State ()
moveDraggedPort evt portDrag = do
    mousePos       <- workspacePosition evt
    mayDraggedPort <- getPort $ portDrag ^. Action.portDragPortRef
    withJust mayDraggedPort $ \draggedPort -> Global.modifyNodeEditor $
        NodeEditor.draggedPort ?= DraggedPort draggedPort mousePos

restoreConnect :: MouseEvent -> PortDrag -> Command State ()
restoreConnect evt portDrag = do
    Connect.startConnecting (portDrag ^. Action.portDragStartPos) (portDrag ^. Action.portDragPortRef) Nothing (portDrag ^. Action.portDragMode)
    continue $ Connect.handleMove evt

removePort :: PortDrag -> Command State ()
removePort portDrag = Batch.removePort (portDrag ^. Action.portDragPortRef) >> end portDrag

addPort :: NodeId -> Command State ()
addPort = Batch.addPort

endPortDrag :: PortDrag -> Command State ()
endPortDrag _ = do
    Global.modifyNodeEditor $ NodeEditor.draggedPort .= Nothing
    removeActionFromState portDragAction
