{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Edge
    ( handleMove
    , restoreConnect
    , removePort
    , addPort
    , endPortDrag
    ) where

import           Empire.API.Data.Node               (NodeId)
import qualified Empire.API.Data.PortRef            as PortRef
import qualified Luna.Studio.Action.Batch           as Batch
import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.Action.Connect         as Connect
import           Luna.Studio.Action.Graph.Lookup    (getPort)
import           Luna.Studio.Event.Mouse            (mousePosition, workspacePosition)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.React.Model.Port       (DraggedPort (DraggedPort))
import qualified Luna.Studio.React.Model.Port       as Port
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

handleMove :: MouseEvent -> NodeId -> Command State ()
handleMove evt nodeId = do
    continue $ connectOrPortDrag evt nodeId
    continue $ moveDraggedPort   evt

connectOrPortDrag :: MouseEvent -> NodeId -> Connect -> Command State ()
connectOrPortDrag evt nodeId connect =
    if ( connect ^. Action.connectIsConnModified
      || connect ^. Action.connectSourcePort . PortRef.nodeId /= nodeId) then
         continue $ Connect.handleMove evt
    else begin $ PortDrag (connect ^. Action.connectStartPos) (connect ^. Action.connectSourcePort) (connect ^. Action.connectMode)

moveDraggedPort :: MouseEvent -> PortDrag -> Command State ()
moveDraggedPort evt portDrag = do
    let nodeId = portDrag ^. Action.portDragPortRef . PortRef.nodeId
        portId = portDrag ^. Action.portDragPortRef . PortRef.portId
    mousePos       <- mousePosition evt
    mayDraggedPort <- getPort $ portDrag ^. Action.portDragPortRef
    withJust mayDraggedPort $ \draggedPort -> Global.modifyNodeEditor $ do
        NodeEditor.draggedPort ?= DraggedPort draggedPort mousePos
        NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.visible .= False

restoreConnect :: MouseEvent -> PortDrag -> Command State ()
restoreConnect evt portDrag = do
    Connect.startConnecting (portDrag ^. Action.portDragStartPos) (portDrag ^. Action.portDragPortRef) Nothing (portDrag ^. Action.portDragMode)
    continue $ Connect.handleMove evt

removePort :: PortDrag -> Command State ()
removePort portDrag = Batch.removePort (portDrag ^. Action.portDragPortRef) >> end portDrag

addPort :: NodeId -> Command State ()
addPort = Batch.addPort

endPortDrag :: PortDrag -> Command State ()
endPortDrag portDrag = do
    let nodeId = portDrag ^. Action.portDragPortRef . PortRef.nodeId
        portId = portDrag ^. Action.portDragPortRef . PortRef.portId
    Global.modifyNodeEditor $ do
        NodeEditor.draggedPort .= Nothing
        NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.visible   .= True
        NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.highlight .= False
    removeActionFromState portDragAction
