{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Luna.Studio.Action.Edge
    ( startPortDrag
    , handleAppMove
    , handleEdgeMove
    , handleMouseUp
    , stopPortDrag
    , removePort
    , addPort
    , portRename
    , portNameEdit
    ) where

import           Control.Arrow
import           Control.Monad.Trans.Maybe              (MaybeT (MaybeT), runMaybeT)
import qualified Data.HashMap.Strict                    as HashMap
import           Data.Map.Lazy                          (Map)
import qualified Data.Map.Lazy                          as Map
import           Data.Position                          (Position (Position), move)
import           Data.ScreenPosition                    (ScreenPosition, fromScreenPosition)
import           Data.Size                              (x, y)
import           Data.Vector                            (Vector2 (Vector2), scalarProduct)
import qualified Empire.API.Data.Connection             as ConnectionAPI
import           Empire.API.Data.Node                   (NodeId)
import qualified Empire.API.Data.Node                   as NodeAPI
import           Empire.API.Data.Port                   (InPort (Arg, Self), OutPort (All, Projection), PortId (InPortId, OutPortId),
                                                         _InPortId, _OutPortId)
import qualified Empire.API.Data.Port                   as PortAPI
import           Empire.API.Data.PortRef                (AnyPortRef (InPortRef', OutPortRef'), toAnyPortRef)
import qualified Empire.API.Data.PortRef                as PortRef
import qualified Luna.Studio.Action.Batch               as Batch
import           Luna.Studio.Action.Camera.Screen       (getInputSidebarPosition, getInputSidebarSize, getOutputSidebarPosition,
                                                         getOutputSidebarSize, translateToWorkspace)
import           Luna.Studio.Action.Command             (Command)
import qualified Luna.Studio.Action.Connect             as Connect
import           Luna.Studio.Action.Geometry            (lineHeight)
import           Luna.Studio.Action.Geometry.Connection (createConnectionModel, createCurrentConnectionModel, getInputEdgePortPosition,
                                                         getOutputEdgePortPosition)
import qualified Luna.Studio.Action.Graph.AddPort       as AddPort
import           Luna.Studio.Action.Graph.Lookup        (getPort)
import           Luna.Studio.Action.Graph.Update        (updateConnectionsForEdges)
import           Luna.Studio.Event.Mouse                (mousePosition, workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection     (toConnection)
import qualified Luna.Studio.React.Model.Connection     as Connection
import           Luna.Studio.React.Model.Node           (Node, fromNode, isInputEdge)
import qualified Luna.Studio.React.Model.Node           as Node
import qualified Luna.Studio.React.Model.NodeEditor     as NodeEditor
import           Luna.Studio.React.Model.Port           (DraggedPort (DraggedPort))
import qualified Luna.Studio.React.Model.Port           as Port
import qualified Luna.Studio.React.View.Edge            as Edge
import           Luna.Studio.State.Action               (Action (begin, continue, end, update), Connect, Mode (Click, Drag),
                                                         PortDrag (PortDrag), portDragAction)
import qualified Luna.Studio.State.Action               as Action
import           Luna.Studio.State.Global               (State, beginActionWithKey, continueActionWithKey, getNode, removeActionFromState,
                                                         updateActionWithKey)
import qualified Luna.Studio.State.Global               as Global
import qualified Luna.Studio.State.Graph                as Graph
import           React.Flux                             (MouseEvent)


instance Action (Command State) PortDrag where
    begin    = beginActionWithKey    portDragAction
    continue = continueActionWithKey portDragAction
    update   = updateActionWithKey   portDragAction
    end      = stopPortDrag

portRename :: AnyPortRef -> String -> Command State ()
portRename portRef name = Global.modifyNodeEditor $ do
    let nodeId = portRef ^. PortRef.nodeId
        portId = portRef ^. PortRef.portId
    NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.name .= name

portNameEdit :: AnyPortRef -> Bool -> Command State ()
portNameEdit portRef isEdited = do
    Global.modifyNodeEditor $ do
        let nodeId = portRef ^. PortRef.nodeId
            portId = portRef ^. PortRef.portId
        NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.isEdited .= isEdited
    when isEdited $ do
        Global.renderIfNeeded
        liftIO Edge.focusPortLabel

startPortDrag :: ScreenPosition -> AnyPortRef -> Mode -> Command State ()
startPortDrag mousePos portRef mode = do
    mayDraggedPort <- getPort portRef
    mayNode        <- getNode $ portRef ^. PortRef.nodeId
    withJust ((,) <$> mayDraggedPort <*> mayNode) $ \(draggedPort, node) -> do
        let nodeId = portRef ^. PortRef.nodeId
            portId = portRef ^. PortRef.portId
        mayDraggedPortPos <- getDraggedPortPositionInSidebar mousePos node
        withJust mayDraggedPortPos $ \draggedPortPos -> do
            let portMapping = Map.fromList $ map (id &&& id) $ node ^. Node.ports . to Map.keys
            begin $ PortDrag mousePos portRef portMapping mode
            Global.modifyNodeEditor $ do
                NodeEditor.draggedPort ?= DraggedPort draggedPort draggedPortPos
                NodeEditor.nodes . at nodeId . _Just . Node.ports . at portId . _Just . Port.visible .= False
            translateToWorkspace mousePos >>= updateConnectionsForDraggedPort portRef

handleMouseUp :: MouseEvent -> PortDrag -> Command State ()
handleMouseUp evt portDrag = do
    mousePos <- mousePosition evt
    if ( portDrag ^. Action.portDragMode == Drag
      && mousePos == portDrag ^. Action.portDragStartPos ) then
        update $ portDrag & Action.portDragMode .~ Click
    else confirmReorder portDrag >> end portDrag

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
    let portRef = portDrag ^. Action.portDragPortRef
        nodeId  = portRef  ^. PortRef.nodeId
        portId  = portRef  ^. PortRef.portId
    apiNode        <- HashMap.lookup nodeId <$> (use $ Global.graph . Graph.nodesMap)
    let mayOriginalNode = fromNode <$> apiNode
    withJust mayOriginalNode $ \node' -> do
        let node = node' & Node.ports . at portId . _Just . Port.visible .~ False
        mousePos <- mousePosition evt
        workspaceMousePos <- workspacePosition evt
        mayDraggedPortPos <- getDraggedPortPositionInSidebar mousePos node
        case mayDraggedPortPos of
            Nothing -> end portDrag
            Just draggedPortPos -> do
                Global.modifyNodeEditor $ NodeEditor.draggedPort . _Just . Port.positionInSidebar .= draggedPortPos
                localReorderPorts workspaceMousePos node portDrag
                updateConnectionsForDraggedPort portRef workspaceMousePos

stopPortDrag :: PortDrag -> Command State ()
stopPortDrag portDrag = do
    let portRef = portDrag ^. Action.portDragPortRef
        nodeId  = portRef ^. PortRef.nodeId
    Global.modifyNodeEditor $ do
        NodeEditor.draggedPort         .= Nothing
        NodeEditor.portDragConnections .= def
    mayNode <- use $ Global.graph . Graph.nodesMap . at nodeId
    withJust mayNode $ \node -> Global.modifyNodeEditor $ NodeEditor.nodes . at nodeId ?= fromNode node
    updateConnectionsForEdges
    removeActionFromState portDragAction

restoreConnect :: PortDrag -> Command State ()
restoreConnect portDrag =
    Connect.startConnecting (portDrag ^. Action.portDragStartPos) (portDrag ^. Action.portDragPortRef) Nothing (portDrag ^. Action.portDragMode)

restorePortDrag :: NodeId -> Connect -> Command State ()
restorePortDrag nodeId connect = when (connect ^. Action.connectSourcePort . PortRef.nodeId == nodeId) $ do
    startPortDrag (connect ^. Action.connectStartPos) (connect ^. Action.connectSourcePort) (connect ^. Action.connectMode)

removePort :: PortDrag -> Command State ()
removePort portDrag = Batch.removePort (portDrag ^. Action.portDragPortRef) >> end portDrag

addPort :: AnyPortRef -> Command State ()
addPort = AddPort.addPort



getDraggedPortPositionInSidebar :: ScreenPosition -> Node -> Command State (Maybe Position)
getDraggedPortPositionInSidebar mousePos node = runMaybeT $ do
    sidebarPos  <- MaybeT $ if isInputEdge node then getInputSidebarPosition else getOutputSidebarPosition
    sidebarSize <- MaybeT $ if isInputEdge node then getInputSidebarSize     else getOutputSidebarSize
    let shift = flip scalarProduct (-1) $ Vector2 (sidebarPos ^. x) (sidebarPos ^. y + sidebarSize ^. y / 2)
    return $ move shift $ Position $ fromScreenPosition mousePos

updateConnectionsForPort :: PortDrag -> AnyPortRef -> Command State ()
updateConnectionsForPort portDrag portRef = do
    graph <- use Global.graph
    let portMapping = portDrag ^. Action.portDragPortMapping
        connectionsToUpdate = Graph.connectionsContainingPort portRef graph
    forM_ connectionsToUpdate $ \conn -> do
        let mayNewConn = case portRef of
                InPortRef' _ -> do
                    newDstPortId <- Map.lookup (portRef ^. PortRef.portId) portMapping
                    newDstPort   <- newDstPortId ^? _InPortId
                    return $ conn & ConnectionAPI.dst . PortRef.dstPortId .~ newDstPort
                OutPortRef' _ -> do
                    newSrcPortId <- Map.lookup (portRef ^. PortRef.portId) portMapping
                    newSrcPort   <- newSrcPortId ^? _OutPortId
                    return $ conn & ConnectionAPI.src . PortRef.srcPortId .~ newSrcPort
        withJust mayNewConn $ \newConn -> do
            mayConnectionModel <- createConnectionModel newConn
            Global.modifyNodeEditor $ NodeEditor.connections . at (newConn ^. ConnectionAPI.dst) .= mayConnectionModel


updateConnectionsForDraggedPort :: AnyPortRef -> Position -> Command State ()
updateConnectionsForDraggedPort portRef pos = do
    graph <- use Global.graph
    forM_ (Graph.connectionsContainingPort portRef graph) $ \conn -> do
        let connId = conn ^. ConnectionAPI.dst
            srcPortRef = case portRef of
                InPortRef'  _ -> OutPortRef' $ conn ^. ConnectionAPI.src
                OutPortRef' _ -> InPortRef'  $ conn ^. ConnectionAPI.dst
        mayConnModel <- (fmap . fmap) (toConnection connId) $ createCurrentConnectionModel srcPortRef pos
        mayPortColor <- (fmap . fmap) (view Port.color) $ getPort portRef
        withJust ((,) <$> mayConnModel <*> mayPortColor) $ \(connModel', portColor) -> do
            let connModel = case portRef of
                    OutPortRef' _ -> connModel' & Connection.color .~ portColor
                    InPortRef'  _ -> connModel'
            Global.modifyNodeEditor $ do
                NodeEditor.connections         . at connId .= Nothing
                NodeEditor.portDragConnections . at connId ?= connModel

getPortPositionToCompare :: PortId -> Position -> PortDrag -> Command State (Maybe Position)
getPortPositionToCompare portId mousePos portDrag = do
    let draggedPortId = portDrag ^. Action.portDragPortRef . PortRef.portId
    if portId == draggedPortId then
            return $ Just $ move (Vector2 0 (-lineHeight/2)) mousePos
        else do
            let draggedPortNum = case draggedPortId of
                    InPortId  Self             -> -1
                    InPortId  (Arg num)        -> num
                    OutPortId All              -> -1
                    OutPortId (Projection num) -> num
            case portId of
                InPortId  Self             -> getOutputEdgePortPosition 0   True
                InPortId  (Arg num)        -> if num < draggedPortNum then orgPos
                    else (fmap . fmap) (move (Vector2 0 (-lineHeight))) orgPos where
                        orgPos = getOutputEdgePortPosition num False
                OutPortId All              -> $notImplemented
                OutPortId (Projection num) -> if num < draggedPortNum then orgPos
                    else (fmap . fmap) (move (Vector2 0 (-lineHeight))) orgPos where
                        orgPos = getInputEdgePortPosition num

comparePortsByNewPositionInNode :: (PortId, Position) -> (PortId, Position) -> Ordering
comparePortsByNewPositionInNode (portId1, pos1) (portId2, pos2) = case (portId1, portId2) of
    ((InPortId (Arg _))        , (InPortId (Arg _)))         -> compare (pos1 ^. y) (pos2 ^. y)
    ((OutPortId (Projection _)), (OutPortId (Projection _))) -> compare (pos1 ^. y) (pos2 ^. y)
    _ -> compare portId1 portId2

-- reorderPortsInNode :: Node -> Map PortId PortId -> Maybe Node
reorderPortsInNode :: forall n p . Lens' n (Map PortId p) -> Lens' p PortId -> n -> Map PortId PortId -> Maybe n
reorderPortsInNode ports portId node portMapping = do
    let ports' = node ^. ports . to Map.elems
    newPorts <- forM ports' $ \port -> do
            newId <- Map.lookup (port ^. portId) portMapping
            return $ port & portId .~ newId
    return $ node & ports .~ (Map.fromList $ map (view portId &&& id) newPorts)

localReorderPorts :: Position -> Node -> PortDrag -> Command State ()
localReorderPorts mousePos originalNode portDrag = do
    let portsIds = originalNode ^. Node.ports . to Map.keys
        nodeId   = originalNode ^. Node.nodeId
    mayPortsWithPos <- fmap sequence $ forM portsIds $ \portId ->
        (fmap . fmap) (portId,) $ getPortPositionToCompare portId mousePos portDrag
    case mayPortsWithPos of
        Nothing -> end portDrag
        Just portsWithPos -> do
            let newOrder   = map fst $ sortBy comparePortsByNewPositionInNode portsWithPos
                newMapping = Map.fromList $ zip newOrder portsIds
            case reorderPortsInNode Node.ports Port.portId originalNode newMapping of
                Nothing          -> end portDrag
                Just updatedNode -> do
                    Global.modifyNodeEditor $ NodeEditor.nodes . at nodeId ?= updatedNode
                    update $ portDrag & Action.portDragPortMapping .~ newMapping
                    continue $ \portDrag' -> mapM_ (updateConnectionsForPort portDrag') $
                        map (toAnyPortRef (originalNode ^. Node.nodeId) . view Port.portId) (originalNode ^. Node.ports . to Map.elems)

confirmReorder :: PortDrag -> Command State ()
confirmReorder portDrag = do
    let portMapping  = portDrag ^. Action.portDragPortMapping
        portRef      = portDrag ^. Action.portDragPortRef
        nodeId       = portRef  ^. PortRef.nodeId
        portId       = portRef  ^. PortRef.portId
        mayNewPortId = Map.lookup portId portMapping
    withJust mayNewPortId $ \newPortId -> when (newPortId /= portId) $ do
        graph   <- use Global.graph
        let mayOldNode    = HashMap.lookup nodeId $ graph ^. Graph.nodesMap
            mayPortNewPos = case newPortId of
                OutPortId (Projection num) -> Just num
                InPortId  (Arg num)        -> Just num
                _                          -> Nothing
        case (,) <$> mayOldNode <*> mayPortNewPos of
            Nothing   -> end portDrag
            Just (node', num) -> do
                let mayNode = reorderPortsInNode NodeAPI.ports PortAPI.portId node' portMapping
                    connectionsToUpdate   = Graph.connectionsContainingNode nodeId graph
                    mayUpdatedConnections = forM connectionsToUpdate $ \conn -> do
                        if | conn ^. ConnectionAPI.src . PortRef.srcNodeId == nodeId -> do
                                let connPortId = OutPortId (conn ^. ConnectionAPI.src . PortRef.srcPortId)
                                newConnPortId <- Map.lookup connPortId portMapping
                                newPort       <- newConnPortId ^? _OutPortId
                                return $ conn & ConnectionAPI.src . PortRef.srcPortId .~ newPort
                           | conn ^. ConnectionAPI.dst . PortRef.dstNodeId == nodeId -> do
                               let connPortId = InPortId (conn ^. ConnectionAPI.dst . PortRef.dstPortId)
                               newConnPortId <- Map.lookup connPortId portMapping
                               newPort <- newConnPortId ^? _InPortId
                               return $ conn & ConnectionAPI.dst . PortRef.dstPortId .~ newPort
                           | otherwise -> Nothing
                case (,) <$> mayNode <*> mayUpdatedConnections of
                    Nothing                 -> end portDrag
                    Just (node, updatedConnections) -> do
                        Global.graph . Graph.nodesMap . at nodeId ?= node
                        forM_ connectionsToUpdate $ \conn ->
                            Global.graph . Graph.connectionsMap . at (conn ^. ConnectionAPI.dst) .= Nothing
                        forM_ updatedConnections $ \conn ->
                            Global.graph . Graph.connectionsMap . at (conn ^. ConnectionAPI.dst) ?= conn
                        Global.modifyNodeEditor $ NodeEditor.nodes . at nodeId ?= fromNode node
                        updateConnectionsForEdges
                        Batch.movePort portRef $notImplemented
