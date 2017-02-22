{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
module Luna.Studio.Action.Edge
    ( startPortDrag
    , restoreConnect
    , restorePortDrag
    , handleMove
    , handleMouseUp
    , stopPortDrag
    , removePort
    , addPort
    ) where


import qualified Data.Map.Lazy                      as Map
import           Data.Position                      (Position (Position), move)
import           Data.ScreenPosition                (ScreenPosition, fromScreenPosition)
import           Data.Size                          (y)
import           Data.Vector                        (Vector2 (Vector2))
import           Empire.API.Data.Node               (NodeId)
import           Empire.API.Data.Port               (OutPort (Projection), PortId (OutPortId))
import           Empire.API.Data.PortRef            (AnyPortRef)
import qualified Empire.API.Data.PortRef            as PortRef
import qualified JS.Scene                           as Scene
import qualified Luna.Studio.Action.Batch           as Batch
import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.Action.Connect         as Connect
import           Luna.Studio.Action.Geometry        (lineHeight)
import           Luna.Studio.Action.Graph.Lookup    (getPort)
import           Luna.Studio.Event.Mouse            (mousePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node       (isOutputEdge)
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.React.Model.Port       (DraggedPort (DraggedPort))
import qualified Luna.Studio.React.Model.Port       as Port
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), Connect, Mode (Click, Drag),
                                                     PortDrag (PortDrag), portDragAction)
import qualified Luna.Studio.State.Action           as Action
import           Luna.Studio.State.Global           (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                     updateActionWithKey)
import qualified Luna.Studio.State.Global           as Global
import           React.Flux                         (MouseEvent)


instance Action (Command State) PortDrag where
    begin    = beginActionWithKey    portDragAction
    continue = continueActionWithKey portDragAction
    update   = updateActionWithKey   portDragAction
    end      = stopPortDrag

--TODO[LJK]: Make this work correctly
getDraggedPortPositionInSidebar :: ScreenPosition -> Command State Position
getDraggedPortPositionInSidebar mousePos = do
    sceneHeight <- use $ Global.scene . Scene.size . y
    return $ move (Vector2 0 (-sceneHeight/2)) $ Position $ fromScreenPosition mousePos


startPortDrag :: MouseEvent -> AnyPortRef -> Mode -> Command State ()
startPortDrag evt portRef mode = do
    mayDraggedPort <- getPort portRef
    withJust mayDraggedPort $ \draggedPort -> do
        let nodeId = portRef ^. PortRef.nodeId
            portId = portRef ^. PortRef.portId
        mousePos <- mousePosition evt
        draggedPortPos <- getDraggedPortPositionInSidebar mousePos
        begin $ PortDrag mousePos portRef mode
        Global.modifyNodeEditor $ do
            NodeEditor.draggedPort ?= DraggedPort draggedPort draggedPortPos
            NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.visible .= False

handleMouseUp :: MouseEvent -> PortDrag -> Command State ()
handleMouseUp evt portDrag = do
    mousePos <- mousePosition evt
    if ( portDrag ^. Action.portDragMode == Drag
      && mousePos == portDrag ^. Action.portDragStartPos ) then
        update $ portDrag & Action.portDragMode .~ Click
    else end portDrag

handleMove :: MouseEvent -> PortDrag -> Command State ()
handleMove evt portDrag = do
    let nodeId = portDrag ^. Action.portDragPortRef . PortRef.nodeId
        portId = portDrag ^. Action.portDragPortRef . PortRef.portId
    mousePos       <- mousePosition evt
    draggedPortPos <- getDraggedPortPositionInSidebar mousePos
    mayDraggedPort <- getPort $ portDrag ^. Action.portDragPortRef
    withJust mayDraggedPort $ \draggedPort -> Global.modifyNodeEditor $ do
        NodeEditor.draggedPort ?= DraggedPort draggedPort draggedPortPos
        NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.visible .= False

movePort :: AnyPortRef -> Command State ()
movePort portRef = do
    mayNode <- Global.getNode $ portRef ^. PortRef.nodeId
    mayDraggedPort <- view NodeEditor.draggedPort <$> Global.getNodeEditor
    withJust ((,) <$> mayNode <*> mayDraggedPort) $ \(node, draggedPort) -> do
        let newPos' = (round $ draggedPort ^. Port.positionInSidebar . y / lineHeight) - if isOutputEdge node then 1 else 0
            --TODO[LJK]: Handle this better
            portsIds = flip filter (node ^. Node.ports . to Map.keys) $ \portId -> case portId of
                OutPortId (Projection _) -> True
                _                        -> False
            newPos = max 0 $ min newPos' $ length portsIds - 1
        case portRef ^. PortRef.portId of
            OutPortId (Projection pos) -> if pos /= newPos then
                    Batch.movePort portRef newPos
                else return ()
            _ -> Batch.movePort portRef newPos

stopPortDrag :: PortDrag -> Command State ()
stopPortDrag portDrag = do
    let portRef = portDrag ^. Action.portDragPortRef
        nodeId  = portRef ^. PortRef.nodeId
        portId  = portRef ^. PortRef.portId
    movePort portRef
    Global.modifyNodeEditor $ do
        NodeEditor.draggedPort .= Nothing
        NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.visible   .= True
        NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.highlight .= False
    removeActionFromState portDragAction


restoreConnect :: MouseEvent -> PortDrag -> Command State ()
restoreConnect evt portDrag = do
    Connect.startConnecting (portDrag ^. Action.portDragStartPos) (portDrag ^. Action.portDragPortRef) Nothing (portDrag ^. Action.portDragMode)
    continue $ Connect.handleMove evt

restorePortDrag :: MouseEvent -> NodeId -> Connect -> Command State ()
restorePortDrag evt nodeId connect = when (connect ^. Action.connectSourcePort . PortRef.nodeId == nodeId) $ do
    begin $ PortDrag (connect ^. Action.connectStartPos) (connect ^. Action.connectSourcePort) (connect ^. Action.connectMode)
    continue $ handleMove evt

removePort :: PortDrag -> Command State ()
removePort portDrag = Batch.removePort (portDrag ^. Action.portDragPortRef) >> end portDrag

addPort :: NodeId -> Command State ()
addPort = Batch.addPort
