{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
module Luna.Studio.Action.Edge
    ( startPortDrag
    , handleAppMove
    , handleEdgeMove
    , handleMouseUp
    , stopPortDrag
    , removePort
    , addPort
    ) where


import qualified Data.Map.Lazy                      as Map
import           Data.Position                      (Position (Position), move)
import           Data.ScreenPosition                (ScreenPosition, fromScreenPosition)
import           Data.Size                          (x, y)
import           Data.Vector                        (Vector2 (Vector2), scalarProduct)
import           Empire.API.Data.Node               (NodeId)
import           Empire.API.Data.Port               (InPort (Arg), OutPort (Projection), PortId (InPortId, OutPortId))
import           Empire.API.Data.PortRef            (AnyPortRef)
import qualified Empire.API.Data.PortRef            as PortRef
import qualified Luna.Studio.Action.Batch           as Batch
import           Luna.Studio.Action.Camera.Screen   (getInputSidebarPosition, getInputSidebarSize, getOutputSidebarPosition,
                                                     getOutputSidebarSize)
import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.Action.Connect         as Connect
import           Luna.Studio.Action.Geometry        (lineHeight)
import           Luna.Studio.Action.Graph.Lookup    (getPort)
import           Luna.Studio.Event.Mouse            (mousePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node       (Node, isInputEdge, isOutputEdge)
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.React.Model.Port       (DraggedPort (DraggedPort))
import qualified Luna.Studio.React.Model.Port       as Port
import           Luna.Studio.State.Action           (Action (begin, continue, end, update), Connect, Mode (Click, Drag),
                                                     PortDrag (PortDrag), portDragAction)
import qualified Luna.Studio.State.Action           as Action
import           Luna.Studio.State.Global           (State, beginActionWithKey, continueActionWithKey, getNode, removeActionFromState,
                                                     updateActionWithKey)
import qualified Luna.Studio.State.Global           as Global
import           React.Flux                         (MouseEvent)

instance Action (Command State) PortDrag where
    begin    = beginActionWithKey    portDragAction
    continue = continueActionWithKey portDragAction
    update   = updateActionWithKey   portDragAction
    end      = stopPortDrag


getDraggedPortPositionInSidebar :: ScreenPosition -> Node -> Command State (Maybe Position)
getDraggedPortPositionInSidebar mousePos node = do
    maySidebarPos  <- if isInputEdge node then getInputSidebarPosition else getOutputSidebarPosition
    maySidebarSize <- if isInputEdge node then getInputSidebarSize     else getOutputSidebarSize
    case (,) <$> maySidebarSize <*> maySidebarPos of
        Just (sidebarSize, sidebarPos) -> do
            let shift = flip scalarProduct (-1) $ Vector2 (sidebarPos ^. x) (sidebarPos ^. y + sidebarSize ^. y / 2)
            return $ Just $ move shift $ Position $ fromScreenPosition mousePos
        Nothing -> return Nothing


startPortDrag :: MouseEvent -> AnyPortRef -> Mode -> Command State ()
startPortDrag evt portRef mode = do
    mayDraggedPort <- getPort portRef
    mayNode        <- getNode $ portRef ^. PortRef.nodeId
    withJust ((,) <$> mayDraggedPort <*> mayNode) $ \(draggedPort, node) -> do
        let nodeId = portRef ^. PortRef.nodeId
            portId = portRef ^. PortRef.portId
        mousePos <- mousePosition evt
        mayDraggedPortPos <- getDraggedPortPositionInSidebar mousePos node
        withJust mayDraggedPortPos $ \draggedPortPos -> do
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
    else movePort (portDrag ^. Action.portDragPortRef) >> end portDrag

handleEdgeMove :: MouseEvent -> NodeId -> Command State ()
handleEdgeMove evt nodeId = do
    continue $ restorePortDrag nodeId
    continue $ handleMove evt

handleAppMove :: MouseEvent -> Command State ()
handleAppMove evt = do
    continue $ restoreConnect
    continue $ Connect.handleMove evt

handleMove :: MouseEvent -> PortDrag -> Command State ()
handleMove evt portDrag = do
    let nodeId = portDrag ^. Action.portDragPortRef . PortRef.nodeId
        portId = portDrag ^. Action.portDragPortRef . PortRef.portId
    mayDraggedPort <- getPort $ portDrag ^. Action.portDragPortRef
    mayNode        <- getNode $ portDrag ^. Action.portDragPortRef . PortRef.nodeId
    withJust ((,) <$> mayDraggedPort <*> mayNode) $ \(draggedPort, node) -> do
        mousePos       <- mousePosition evt
        mayDraggedPortPos <- getDraggedPortPositionInSidebar mousePos node
        case mayDraggedPortPos of
            Just draggedPortPos -> Global.modifyNodeEditor $ do
                NodeEditor.draggedPort ?= DraggedPort draggedPort draggedPortPos
                NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.visible .= False
            Nothing -> end portDrag

getNumOfProjectionsOrArgs :: Node -> Int
getNumOfProjectionsOrArgs node =
    length $ flip filter (node ^. Node.ports . to Map.keys) $ \portId ->
        if isInputEdge node then
            case portId of
                OutPortId (Projection _) -> True
                _                        -> False
            else case portId of
                InPortId (Arg _) -> True
                _                -> False

movePort :: AnyPortRef -> Command State ()
movePort portRef = do
    mayNode <- Global.getNode $ portRef ^. PortRef.nodeId
    mayDraggedPort <- view NodeEditor.draggedPort <$> Global.getNodeEditor
    withJust ((,) <$> mayNode <*> mayDraggedPort) $ \(node, draggedPort) -> do
        let newPos' = (round $ draggedPort ^. Port.positionInSidebar . y / lineHeight) - if isOutputEdge node then 1 else 0
            numOfPorts = getNumOfProjectionsOrArgs node
            newPos = max 0 $ min newPos' $ numOfPorts - 1
        case portRef ^. PortRef.portId of
            OutPortId (Projection pos) -> when (pos /= newPos) $
                Batch.movePort portRef newPos
            InPortId  (Arg pos)        -> when (pos /= newPos) $
                Batch.movePort portRef newPos
            _ -> return ()

stopPortDrag :: PortDrag -> Command State ()
stopPortDrag portDrag = do
    let portRef = portDrag ^. Action.portDragPortRef
        nodeId  = portRef ^. PortRef.nodeId
        portId  = portRef ^. PortRef.portId
    Global.modifyNodeEditor $ do
        NodeEditor.draggedPort .= Nothing
        NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.visible   .= True
        NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.highlight .= False
    removeActionFromState portDragAction

restoreConnect :: PortDrag -> Command State ()
restoreConnect portDrag =
    Connect.startConnecting (portDrag ^. Action.portDragStartPos) (portDrag ^. Action.portDragPortRef) Nothing (portDrag ^. Action.portDragMode)

restorePortDrag :: NodeId -> Connect -> Command State ()
restorePortDrag nodeId connect = when (connect ^. Action.connectSourcePort . PortRef.nodeId == nodeId) $ do
    begin $ PortDrag (connect ^. Action.connectStartPos) (connect ^. Action.connectSourcePort) (connect ^. Action.connectMode)

removePort :: PortDrag -> Command State ()
removePort portDrag = Batch.removePort (portDrag ^. Action.portDragPortRef) >> end portDrag

addPort :: NodeId -> Command State ()
addPort = Batch.addPort
