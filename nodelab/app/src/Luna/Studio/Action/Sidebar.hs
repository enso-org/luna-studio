{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Luna.Studio.Action.Sidebar where

import           Control.Monad.Trans.Maybe                (MaybeT (MaybeT), runMaybeT)
import           Data.ScreenPosition                      (ScreenPosition)
import           Data.Size                                (y)
import qualified JS.Scene                                 as Scene
import           Luna.Studio.Action.Basic                 (getScene, localMovePort, localRemovePort, redrawConnectionsForNode,
                                                           setSidebarPortMode)
import qualified Luna.Studio.Action.Basic                 as Basic
import qualified Luna.Studio.Action.Batch                 as Batch
import           Luna.Studio.Action.Command               (Command)
import qualified Luna.Studio.Action.Connect               as Connect
import           Luna.Studio.Action.State.Action          (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                           updateActionWithKey)
import           Luna.Studio.Action.State.App             (renderIfNeeded)
import           Luna.Studio.Action.State.Model           (getPortPositionInInputSidebar, getPortPositionInOutputSidebar)
import           Luna.Studio.Action.State.NodeEditor      (getSidebarNode, modifySidebarNode)
import           Luna.Studio.Action.State.Scene           (getInputSidebarSize)
import           Empire.API.Data.PortRef                 (AnyPortRef)
import qualified Empire.API.Data.PortRef                 as PortRef
import           Luna.Studio.Event.Mouse                  (mousePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Constants        (gridSize)
import           Luna.Studio.React.Model.Node.SidebarNode (NodeLoc, SidebarType (Input), countProjectionPorts, fixedBottomPos,
                                                           isOutputSidebar, ports, sidebarType)
import           Luna.Studio.React.Model.Port             (OutPort (Projection), PortId (OutPortId), getPortNumber)
import qualified Luna.Studio.React.Model.Port             as Port
import qualified Luna.Studio.React.View.Sidebar           as Sidebar
import           Luna.Studio.State.Action                 (Action (begin, continue, end, update), Connect, Mode (Click, Drag),
                                                           PortDrag (PortDrag), connectIsPortPhantom, connectMode, connectSourcePort,
                                                           connectStartPos, portDragActPortRef, portDragAction, portDragIsPortPhantom,
                                                           portDragMode, portDragPortStartPosInSidebar, portDragStartPortRef,
                                                           portDragStartPos)
import           Luna.Studio.State.Global                 (State)
import           React.Flux                               (MouseEvent)


instance Action (Command State) PortDrag where
    begin      = beginActionWithKey    portDragAction
    continue   = continueActionWithKey portDragAction
    update     = updateActionWithKey   portDragAction
    end action = do
        if action ^. portDragIsPortPhantom
            then do
                void $ localRemovePort $ action ^. portDragActPortRef
                removeActionFromState portDragAction
            else cancelPortDragUnsafe action

getInputSidebarBottomDistance :: Command State (Maybe Double)
getInputSidebarBottomDistance = getScene >>= \mayScene -> return $
    case (mayScene, (join $ view Scene.inputSidebar <$> mayScene)) of
        (Nothing, _)               -> Nothing
        (_, Nothing)               -> Nothing
        (Just scene, Just sidebar) -> Just $
            scene ^. Scene.size . y - sidebar ^. Scene.inputSidebarPosition . y - sidebar ^. Scene.inputSidebarSize . y

setFixedBottomPos :: AnyPortRef -> Command State ()
setFixedBottomPos portRef = do
    let nodeLoc = portRef ^. PortRef.nodeLoc
    mayNode <- getSidebarNode nodeLoc
    withJust mayNode $ \node -> case (node ^. sidebarType, portRef ^. PortRef.portId) of
        (Input, OutPortId (Projection _ _)) -> do
            mayBottomPos <- getInputSidebarBottomDistance
            modifySidebarNode nodeLoc $ fixedBottomPos .= mayBottomPos
        _ -> $notImplemented

addPort :: AnyPortRef -> Command State ()
addPort portRef = do
    setFixedBottomPos portRef
    Basic.addPort portRef

removePort :: AnyPortRef -> Command State ()
removePort portRef = do
    setFixedBottomPos portRef
    Basic.removePort portRef

startPortNameEdit :: AnyPortRef -> Command State ()
startPortNameEdit portRef = do
    let nodeLoc = portRef ^. PortRef.nodeLoc
        pid     = portRef ^. PortRef.portId
    modifySidebarNode nodeLoc $ ports . ix pid . Port.mode .= Port.NameEdit
    renderIfNeeded
    liftIO Sidebar.focusPortLabel

cancelPortNameEdit :: AnyPortRef -> Command State ()
cancelPortNameEdit portRef = do
    let nodeLoc = portRef ^. PortRef.nodeLoc
        portId  = portRef ^. PortRef.portId
    modifySidebarNode nodeLoc $ ports . ix portId . Port.mode .= Port.Normal

finishPortNameEdit :: AnyPortRef -> String -> Command State ()
finishPortNameEdit portRef name = do
    let nodeLoc = portRef ^. PortRef.nodeLoc
        portId  = portRef ^. PortRef.portId
    Batch.renamePort portRef name
    modifySidebarNode nodeLoc $ ports . ix portId %= (\p -> p & Port.name .~ name
                                                           & Port.mode .~ Port.Normal)

handleMouseUp :: MouseEvent -> PortDrag -> Command State ()
handleMouseUp evt portDrag = do
    mousePos <- mousePosition evt
    if portDrag ^. portDragMode == Click || mousePos /= portDrag ^. portDragStartPos
        then finishPortDrag portDrag
    else if portDrag ^. portDragMode == Drag && mousePos == portDrag ^. portDragStartPos
        then update $ portDrag & portDragMode .~ Click
    else return ()

handleSidebarMove :: MouseEvent -> NodeLoc -> Command State ()
handleSidebarMove evt nodeLoc = do
    continue $ restorePortDrag nodeLoc
    continue $ handleMove evt

handleAppMove :: MouseEvent -> Command State ()
handleAppMove evt = do
    continue $ restoreConnect
    continue $ Connect.handleMove evt

startPortDrag :: ScreenPosition -> AnyPortRef -> Bool -> Mode -> Command State ()
startPortDrag mousePos portRef isPhantom mode = do
    maySuccess <- runMaybeT $ do
        let portId  = portRef ^. PortRef.portId
        node    <- MaybeT $ getSidebarNode $ portRef ^. PortRef.nodeLoc
        portPos <- MaybeT $ if isOutputSidebar node
            then return . Just $ getPortPositionInOutputSidebar portId
            else fmap2 (flip getPortPositionInInputSidebar portId) getInputSidebarSize
        lift . setSidebarPortMode portRef $ Port.Moved portPos
        lift . begin $ PortDrag mousePos portPos portRef portRef isPhantom mode
    when (isNothing maySuccess && isPhantom) $ void $ localRemovePort portRef

handleMove :: MouseEvent -> PortDrag -> Command State ()
handleMove evt portDrag = do
    mousePos <- mousePosition evt
    let portRef       = portDrag ^. portDragActPortRef
        nodeLoc        = portRef  ^. PortRef.nodeLoc
        portId        = portRef  ^. PortRef.portId
        startPortPos  = portDrag ^. portDragPortStartPosInSidebar
        startMousePos = portDrag ^. portDragStartPos
        newPos        = startPortPos & y +~ mousePos ^. y - startMousePos ^. y
    mayNewPortNum <- case portId of
        (OutPortId (Projection i _)) -> runMaybeT $ do
            node <- MaybeT $ getSidebarNode nodeLoc
            let newNum = min (countProjectionPorts node - 1) $ max 0 (round $ newPos ^. y / gridSize)
            if newNum /= i then return newNum else nothing
        _                          -> $notImplemented
    setSidebarPortMode portRef $ Port.Moved newPos
    withJust mayNewPortNum $ \newNum ->
        withJustM (localMovePort portRef newNum) $ \newPortRef ->
            update $ portDrag & portDragActPortRef .~ newPortRef

cancelPortDragUnsafe :: PortDrag -> Command State ()
cancelPortDragUnsafe portDrag = do
    let portRef    = portDrag ^. portDragActPortRef
        orgPortRef = portDrag ^. portDragStartPortRef
        nodeLoc    = portRef ^. PortRef.nodeLoc
        orgPortId  = orgPortRef ^. PortRef.portId
    setSidebarPortMode portRef Port.Normal
    if portRef /= orgPortRef
        then void $ localMovePort portRef $ getPortNumber orgPortId
        else void $ redrawConnectionsForNode nodeLoc
    removeActionFromState portDragAction

finishPortDrag :: PortDrag -> Command State ()
finishPortDrag portDrag = do
    let portRef    = portDrag ^. portDragActPortRef
        orgPortRef = portDrag ^. portDragStartPortRef
        portId     = portRef ^. PortRef.portId
        isPhantom  = portDrag ^. portDragIsPortPhantom
    if portRef == orgPortRef then end portDrag else do
        setSidebarPortMode portRef Port.Normal
        if isPhantom
            then Batch.addPort portRef
            else Batch.movePort orgPortRef $ getPortNumber portId
        removeActionFromState portDragAction

restoreConnect :: PortDrag -> Command State ()
restoreConnect portDrag = do
    cancelPortDragUnsafe portDrag
    Connect.startConnecting (portDrag ^. portDragStartPos) (portDrag ^. portDragStartPortRef) Nothing (portDrag ^. portDragIsPortPhantom) (portDrag ^. portDragMode)

restorePortDrag :: NodeLoc -> Connect -> Command State ()
restorePortDrag nodeLoc connect = when (connect ^. connectSourcePort . PortRef.nodeLoc == nodeLoc) $ do
    Connect.stopConnectingUnsafe connect
    startPortDrag (connect ^. connectStartPos) (connect ^. connectSourcePort) (connect ^. connectIsPortPhantom) (connect ^. connectMode)
