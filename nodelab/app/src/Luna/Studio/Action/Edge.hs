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
import           Luna.Studio.Action.Basic              (localMovePort, redrawConnectionsForNode, setEdgePortMode)
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
import           Luna.Studio.Data.PortRef              (AnyPortRef (InPortRef', OutPortRef'), toAnyPortRef)
import qualified Luna.Studio.Data.PortRef              as PortRef
import           Luna.Studio.Event.Mouse               (mousePosition, workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection    (connectionId, dst, src, toConnection)
import qualified Luna.Studio.React.Model.Connection    as Connection
import           Luna.Studio.React.Model.Constants     (gridSize)
import           Luna.Studio.React.Model.Node.EdgeNode (EdgeNode, NodeLoc, countProjectionPorts, edgeType, isInputEdge, isOutputEdge)
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
    -- let nodeLoc = portRef ^. PortRef.nodeLoc
    --     portId = portRef ^. PortRef.portId
    -- NodeEditor.edgeNodes . at nodeLoc . _Just . EdgeNode.ports . at portId . _Just . Port.name .= name

portNameEdit :: AnyPortRef -> Bool -> Command State ()
portNameEdit portRef isEdited = return () -- do
    -- modifyNodeEditor $ do
    --     let nodeLoc = portRef ^. PortRef.nodeLoc
    --         portId = portRef ^. PortRef.portId
    --     NodeEditor.edgeNodes . at nodeLoc . _Just . EdgeNode.ports . at portId . _Just . Port.isEdited .= isEdited
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

handleEdgeMove :: MouseEvent -> NodeLoc -> Command State ()
handleEdgeMove evt nodeLoc = do
    continue $ restorePortDrag nodeLoc
    continue $ handleMove evt

handleAppMove :: MouseEvent -> Command State ()
handleAppMove evt = do
    continue $ restoreConnect
    continue $ Connect.handleMove evt

startPortDrag :: ScreenPosition -> AnyPortRef -> Mode -> Command State ()
startPortDrag mousePos portRef mode = do
    mayDraggedPort <- getPort portRef
    mayNode        <- getEdgeNode $ portRef ^. PortRef.nodeLoc
    withJust ((,) <$> mayDraggedPort <*> mayNode) $ \(draggedPort, node) -> do
        let nodeLoc = portRef ^. PortRef.nodeLoc
            portId = portRef ^. PortRef.portId
        mayMousePos <- getMousePositionInSidebar mousePos $ node ^. edgeType
        mayPortPos  <- if isOutputEdge node then return . Just $ getPortPositionInOutputSidebar portId
                                            else fmap2 (flip getPortPositionInInputSidebar portId) getInputSidebarSize
        withJust ((,) <$> mayMousePos <*> mayPortPos) $ \(mousePos', portPos) -> do
            let shift = portPos ^. vector - mousePos' ^. vector
            setEdgePortMode portRef $ Port.Moved portPos
            begin $ PortDrag mousePos portPos portRef portRef mode

handleMove :: MouseEvent -> PortDrag -> Command State ()
handleMove evt portDrag = do
    mousePos <- mousePosition evt
    let portRef       = portDrag ^. portDragActPortRef
        nodeLoc        = portRef  ^. PortRef.nodeLoc
        portId        = portRef  ^. PortRef.portId
        startPortPos  = portDrag ^. portDragPortStartPosInSidebar
        startMousePos = portDrag ^. portDragStartPos
        shift         = mousePos ^. vector - startMousePos ^. vector
        newPos        = move shift startPortPos
    mayNewPortNum <- case portId of
        (OutPortId (Projection i)) -> runMaybeT $ do
            node <- MaybeT $ getEdgeNode nodeLoc
            let newNum = min (countProjectionPorts node - 1) $ max 0 (round $ newPos ^. y / gridSize)
            if newNum /= i then return newNum else nothing
        _                          -> $notImplemented
    setEdgePortMode portRef $ Port.Moved newPos
    withJust mayNewPortNum $ \newNum ->
        withJustM (localMovePort portRef newNum) $ \newPortRef ->
            update $ portDrag & portDragActPortRef .~ newPortRef

stopPortDrag :: Bool -> PortDrag -> Command State ()
stopPortDrag acceptChanges portDrag = do
    let portRef    = portDrag ^. portDragActPortRef
        orgPortRef = portDrag ^. portDragStartPortRef
        nodeLoc     = portRef ^. PortRef.nodeLoc
        portId     = portRef ^. PortRef.portId
        orgPortId  = orgPortRef ^. PortRef.portId
    setEdgePortMode portRef Port.Normal
    if portRef /= orgPortRef
        then case (orgPortId, portId) of
            (OutPortId (Projection orgNum), OutPortId (Projection num)) ->
                if acceptChanges
                    then Batch.movePort orgPortRef num
                    else void $ localMovePort portRef orgNum
            _                        -> $notImplemented
        else void $ redrawConnectionsForNode nodeLoc
    removeActionFromState portDragAction


restoreConnect :: PortDrag -> Command State ()
restoreConnect portDrag =
    Connect.startConnecting (portDrag ^. portDragStartPos) (portDrag ^. portDragStartPortRef) Nothing (portDrag ^. portDragMode)

restorePortDrag :: NodeLoc -> Connect -> Command State ()
restorePortDrag nodeLoc connect = when (connect ^. connectSourcePort . PortRef.nodeLoc == nodeLoc) $ do
    startPortDrag (connect ^. connectStartPos) (connect ^. connectSourcePort) (connect ^. connectMode)
