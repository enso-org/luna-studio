{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Luna.Studio.Action.Edge
    ( startPortDrag
    , handleAppMove
    , handleEdgeMove
    , handleMouseUp
    , stopPortDrag
    , portRename
    , portNameEdit
    ) where

import           Control.Arrow
import           Control.Monad.Trans.Maybe             (MaybeT (MaybeT), runMaybeT)
import           Data.Map.Lazy                         (Map)
import qualified Data.Map.Lazy                         as Map
import           Data.Position                         (Position (Position), move, vector)
import           Data.ScreenPosition                   (ScreenPosition, fromScreenPosition)
import           Data.Size                             (x, y)
import           Data.Vector                           (Vector2 (Vector2), scalarProduct)
import           Empire.API.Data.PortRef               (AnyPortRef (InPortRef', OutPortRef'), toAnyPortRef)
import qualified Empire.API.Data.PortRef               as PortRef
import           Luna.Studio.Action.Basic              (localMovePort, redrawConnectionsForNode)
import qualified Luna.Studio.Action.Basic              as Basic
import qualified Luna.Studio.Action.Batch              as Batch
import           Luna.Studio.Action.Command            (Command)
import qualified Luna.Studio.Action.Connect            as Connect
import           Luna.Studio.Action.State.Action       (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                        updateActionWithKey)
import           Luna.Studio.Action.State.App          (renderIfNeeded)
import           Luna.Studio.Action.State.Model        (createConnectionModel, createCurrentConnectionModel, getInputEdgePortPosition,
                                                        getMousePositionInSidebar, getOutputEdgePortPosition, getPortPositionInInputSidebar,
                                                        getPortPositionInOutputSidebar)
import           Luna.Studio.Action.State.NodeEditor   (addConnection, getConnectionsContainingNode, getConnectionsContainingPortRef,
                                                        getEdgeNode, getPort, modifyEdgeNode, modifyNodeEditor, removeConnection)
import           Luna.Studio.Action.State.Scene        (getInputSidebarPosition, getInputSidebarSize, getOutputSidebarPosition,
                                                        getOutputSidebarSize, translateToWorkspace)
import           Luna.Studio.Event.Mouse               (mousePosition, workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection    (connectionId, dst, src, toConnection)
import qualified Luna.Studio.React.Model.Connection    as Connection
import           Luna.Studio.React.Model.Constants     (lineHeight)
import           Luna.Studio.React.Model.Node.EdgeNode (EdgeNode, NodeId, edgeType, isInputEdge, isOutputEdge)
import qualified Luna.Studio.React.Model.Node.EdgeNode as EdgeNode
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.React.Model.Port          (InPort (Arg, Self), OutPort (All, Projection), PortId (InPortId, OutPortId),
                                                        _InPortId, _OutPortId)
import qualified Luna.Studio.React.Model.Port          as Port
import qualified Luna.Studio.React.View.Edge           as Edge
import           Luna.Studio.State.Action              (Action (begin, continue, end, update), Connect, Mode (Click, Drag),
                                                        PortDrag (PortDrag), connectMode, connectSourcePort, connectStartPos,
                                                        portDragActPortRef, portDragAction, portDragMode, portDragPortStartPosInSidebar,
                                                        portDragStartPortRef, portDragStartPos)
import           Luna.Studio.State.Global              (State)
import           React.Flux                            (MouseEvent)


instance Action (Command State) PortDrag where
    begin    = beginActionWithKey    portDragAction
    continue = continueActionWithKey portDragAction
    update   = updateActionWithKey   portDragAction
    end      = stopPortDrag False

portRename :: AnyPortRef -> String -> Command State ()
portRename portRef name = return () -- modifyNodeEditor $ do
    -- let nodeId = portRef ^. PortRef.nodeId
    --     portId = portRef ^. PortRef.portId
    -- NodeEditor.edgeNodes . at nodeId . _Just . EdgeNode.ports . at portId . _Just . Port.name .= name

portNameEdit :: AnyPortRef -> Bool -> Command State ()
portNameEdit portRef isEdited = return () -- do
    -- modifyNodeEditor $ do
    --     let nodeId = portRef ^. PortRef.nodeId
    --         portId = portRef ^. PortRef.portId
    --     NodeEditor.edgeNodes . at nodeId . _Just . EdgeNode.ports . at portId . _Just . Port.isEdited .= isEdited
    -- when isEdited $ do
    --     renderIfNeeded
    --     liftIO Edge.focusPortLabel


handleMouseUp :: MouseEvent -> PortDrag -> Command State ()
handleMouseUp evt portDrag = do
    mousePos <- mousePosition evt
    if ( portDrag ^. portDragMode == Drag
      && mousePos == portDrag ^. portDragStartPos ) then
        update $ portDrag & portDragMode .~ Click
    else stopPortDrag True portDrag

handleEdgeMove :: MouseEvent -> NodeId -> Command State ()
handleEdgeMove evt nodeId = do
    continue $ restorePortDrag nodeId
    continue $ handleMove evt

handleAppMove :: MouseEvent -> Command State ()
handleAppMove evt = do
    continue $ restoreConnect
    continue $ Connect.handleMove evt





startPortDrag :: ScreenPosition -> AnyPortRef -> Mode -> Command State ()
startPortDrag mousePos portRef mode = do
    mayDraggedPort <- getPort portRef
    mayNode        <- getEdgeNode $ portRef ^. PortRef.nodeId
    withJust ((,) <$> mayDraggedPort <*> mayNode) $ \(draggedPort, node) -> do
        let nodeId = portRef ^. PortRef.nodeId
            portId = portRef ^. PortRef.portId
        mayMousePos <- getMousePositionInSidebar mousePos $ node ^. edgeType
        mayPortPos  <- if isOutputEdge node then return . Just $ getPortPositionInOutputSidebar portId
                                            else fmap2 (flip getPortPositionInInputSidebar portId) getInputSidebarSize
        withJust ((,) <$> mayMousePos <*> mayPortPos) $ \(mousePos', portPos) -> do
            let shift = portPos ^. vector - mousePos' ^. vector
            modifyEdgeNode nodeId $ EdgeNode.ports . ix portId . Port.mode .= Port.Moved portPos
            begin $ PortDrag mousePos portPos portRef portRef mode
            print portPos

handleMove :: MouseEvent -> PortDrag -> Command State ()
handleMove evt portDrag = do
    mousePos <- mousePosition evt
    let portRef       = portDrag ^. portDragActPortRef
        nodeId        = portRef  ^. PortRef.nodeId
        portId        = portRef  ^. PortRef.portId
        startPortPos  = portDrag ^. portDragPortStartPosInSidebar
        startMousePos = portDrag ^. portDragStartPos
        shift         = mousePos ^. vector - startMousePos ^. vector
    modifyEdgeNode nodeId $ EdgeNode.ports . ix portId . Port.mode .= Port.Moved (move shift startPortPos)

stopPortDrag :: Bool -> PortDrag -> Command State ()
stopPortDrag acceptChanges portDrag = do
    let portRef    = portDrag ^. portDragActPortRef
        orgPortRef = portDrag ^. portDragStartPortRef
        nodeId     = portRef ^. PortRef.nodeId
        portId     = portRef ^. PortRef.portId
    modifyEdgeNode nodeId $ EdgeNode.ports . ix portId . Port.mode .= Port.Normal
    if portRef /= orgPortRef
        then if acceptChanges
            then Batch.movePort orgPortRef portRef
            else void $ localMovePort portRef orgPortRef
        else void $ redrawConnectionsForNode nodeId
    removeActionFromState portDragAction


restoreConnect :: PortDrag -> Command State ()
restoreConnect portDrag =
    Connect.startConnecting (portDrag ^. portDragStartPos) (portDrag ^. portDragStartPortRef) Nothing (portDrag ^. portDragMode)

restorePortDrag :: NodeId -> Connect -> Command State ()
restorePortDrag nodeId connect = when (connect ^. connectSourcePort . PortRef.nodeId == nodeId) $ do
    startPortDrag (connect ^. connectStartPos) (connect ^. connectSourcePort) (connect ^. connectMode)




updateConnectionsForPort :: PortDrag -> AnyPortRef -> Command State ()
updateConnectionsForPort portDrag portRef = return ()
    -- let portMapping = portDrag ^. portDragPortMapping
    -- connectionsToUpdate <- getConnectionsContainingPortRef portRef
    -- forM_ connectionsToUpdate $ \conn -> do
    --     let mayNewConn = case portRef of
    --             InPortRef' _ -> do
    --                 newDstPortId <- Map.lookup (portRef ^. PortRef.portId) portMapping
    --                 newDstPort   <- newDstPortId ^? _InPortId
    --                 return $ conn & dst . PortRef.dstPortId .~ newDstPort
    --             OutPortRef' _ -> do
    --                 newSrcPortId <- Map.lookup (portRef ^. PortRef.portId) portMapping
    --                 newSrcPort   <- newSrcPortId ^? _OutPortId
    --                 return $ conn & src . PortRef.srcPortId .~ newSrcPort
    --     withJust mayNewConn $ \newConn -> do
    --         mayConnectionModel <- createConnectionModel (newConn ^. src) (newConn ^. dst)
    --         modifyNodeEditor $ NodeEditor.connections . at (newConn ^. dst) .= mayConnectionModel


updateConnectionsForDraggedPort :: AnyPortRef -> Position -> Command State ()
updateConnectionsForDraggedPort portRef pos = return ()
    -- connectionsToUpdate <- getConnectionsContainingPortRef portRef
    -- forM_ connectionsToUpdate $ \conn -> do
    --     let connId  = conn ^. connectionId
    --         srcPortRef = case portRef of
    --             InPortRef'  _ -> OutPortRef' $ conn ^. src
    --             OutPortRef' _ -> InPortRef'  $ conn ^. dst
    --     mayConnModel <- (fmap . fmap) (toConnection (conn ^. src) connId) $ createCurrentConnectionModel srcPortRef pos
    --     mayPortColor <- (fmap . fmap) (view Port.color) $ getPort portRef
    --     withJust ((,) <$> mayConnModel <*> mayPortColor) $ \(connModel', portColor) -> do
    --         let connModel = case portRef of
    --                 OutPortRef' _ -> connModel' & Connection.color .~ portColor
    --                 InPortRef'  _ -> connModel'
    --         modifyNodeEditor $ do
    --             NodeEditor.connections         . at connId .= Nothing
    --             NodeEditor.portDragConnections . at connId ?= connModel

getPortPositionToCompare :: PortId -> Position -> PortDrag -> Command State (Maybe Position)
getPortPositionToCompare portId mousePos portDrag = return Nothing
    -- let draggedPortId = portDrag ^. portDragPortRef . PortRef.portId
    -- if portId == draggedPortId then
    --         return $ Just $ move (Vector2 0 (-lineHeight/2)) mousePos
    --     else do
    --         let draggedPortNum = case draggedPortId of
    --                 InPortId  Self             -> -1
    --                 InPortId  (Arg num)        -> num
    --                 OutPortId All              -> -1
    --                 OutPortId (Projection num) -> num
    --         case portId of
    --             InPortId  Self             -> getOutputEdgePortPosition (InPortId Self)
    --             InPortId  (Arg num)        -> if num < draggedPortNum then orgPos
    --                 else (fmap . fmap) (move (Vector2 0 (-lineHeight))) orgPos where
    --                     orgPos = getOutputEdgePortPosition (InPortId (Arg num))
    --             OutPortId All              -> $notImplemented
    --             OutPortId (Projection num) -> if num < draggedPortNum then orgPos
    --                 else (fmap . fmap) (move (Vector2 0 (-lineHeight))) orgPos where
    --                     orgPos = getInputEdgePortPosition (OutPortId (Projection num))

comparePortsByNewPositionInNode :: (PortId, Position) -> (PortId, Position) -> Ordering
comparePortsByNewPositionInNode (portId1, pos1) (portId2, pos2) = EQ --case (portId1, portId2) of
    -- ((InPortId (Arg _))        , (InPortId (Arg _)))         -> compare (pos1 ^. y) (pos2 ^. y)
    -- ((OutPortId (Projection _)), (OutPortId (Projection _))) -> compare (pos1 ^. y) (pos2 ^. y)
    -- _ -> compare portId1 portId2

-- reorderPortsInNode :: Node -> Map PortId PortId -> Maybe Node
reorderPortsInNode :: forall n p . Lens' n (Map PortId p) -> Lens' p PortId -> n -> Map PortId PortId -> Maybe n
reorderPortsInNode ports portId node portMapping = def
    -- newPorts <- forM ports' $ \port -> do
    --         newId <- Map.lookup (port ^. portId) portMapping
    --         return $ port & portId .~ newId
    -- return $ node & ports .~ (Map.fromList $ map (view portId &&& id) newPorts)

localReorderPorts :: Position -> EdgeNode -> PortDrag -> Command State ()
localReorderPorts mousePos originalNode portDrag = return ()
    -- let portsIds = originalNode ^. EdgeNode.ports . to Map.keys
    --     nodeId   = originalNode ^. EdgeNode.nodeId
    -- mayPortsWithPos <- fmap sequence $ forM portsIds $ \portId ->
    --     (fmap . fmap) (portId,) $ getPortPositionToCompare portId mousePos portDrag
    -- case mayPortsWithPos of
    --     Nothing -> end portDrag
    --     Just portsWithPos -> do
    --         let newOrder   = map fst $ sortBy comparePortsByNewPositionInNode portsWithPos
    --             newMapping = Map.fromList $ zip newOrder portsIds
    --         case reorderPortsInNode EdgeNode.ports Port.portId originalNode newMapping of
    --             Nothing          -> end portDrag
    --             Just updatedNode -> do
    --                 modifyNodeEditor $ NodeEditor.edgeNodes . at nodeId ?= updatedNode
    --                 update $ portDrag & portDragPortMapping .~ newMapping
    --                 continue $ \portDrag' -> mapM_ (updateConnectionsForPort portDrag') $
    --                     map (toAnyPortRef (originalNode ^. EdgeNode.nodeId) . view Port.portId) (originalNode ^. EdgeNode.ports . to Map.elems)

confirmReorder :: PortDrag -> Command State ()
confirmReorder portDrag = return ()
    -- let portMapping  = portDrag ^. portDragPortMapping
    --     portRef      = portDrag ^. portDragPortRef
    --     nodeId       = portRef  ^. PortRef.nodeId
    --     portId       = portRef  ^. PortRef.portId
    --     mayNewPortId = Map.lookup portId portMapping
    -- withJust mayNewPortId $ \newPortId -> when (newPortId /= portId) $ do
    --     mayOldNode <- getEdgeNode nodeId
    --     let mayPortNewPos = case newPortId of
    --             OutPortId (Projection num) -> Just num
    --             InPortId  (Arg num)        -> Just num
    --             _                          -> Nothing
    --     case (,) <$> mayOldNode <*> mayPortNewPos of
    --         Nothing   -> end portDrag
    --         Just (node', _) -> do
    --             connectionsToUpdate <- getConnectionsContainingNode nodeId
    --             let mayNode = reorderPortsInNode EdgeNode.ports Port.portId node' portMapping
    --                 mayUpdatedConnections = forM connectionsToUpdate $ \conn -> do
    --                     if | conn ^. src . PortRef.srcNodeId == nodeId -> do
    --                             let connPortId = OutPortId (conn ^. src . PortRef.srcPortId)
    --                             newConnPortId <- Map.lookup connPortId portMapping
    --                             newPort       <- newConnPortId ^? _OutPortId
    --                             return $ conn & src . PortRef.srcPortId .~ newPort
    --                        | conn ^. dst . PortRef.dstNodeId == nodeId -> do
    --                            let connPortId = InPortId (conn ^. dst . PortRef.dstPortId)
    --                            newConnPortId <- Map.lookup connPortId portMapping
    --                            newPort <- newConnPortId ^? _InPortId
    --                            return $ conn & dst . PortRef.dstPortId .~ newPort
    --                        | otherwise -> Nothing
    --             case (,) <$> mayNode <*> mayUpdatedConnections of
    --                 Nothing                 -> end portDrag
    --                 Just (node, updatedConnections) -> do
    --                     forM_ connectionsToUpdate $ removeConnection . view connectionId
    --                     forM_ updatedConnections $ addConnection
    --                     modifyNodeEditor $ NodeEditor.edgeNodes . at nodeId ?= node
    --                     void redrawConnectionsForEdgeNodes
    --                     Batch.movePort portRef $notImplemented
