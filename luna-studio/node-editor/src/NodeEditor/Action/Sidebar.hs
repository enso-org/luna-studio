{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module NodeEditor.Action.Sidebar where

import           Common.Prelude
import           Control.Monad.Trans.Maybe               (MaybeT (MaybeT), runMaybeT)
import           Data.ScreenPosition                     (ScreenPosition, y)
import qualified Empire.API.Data.PortRef                 as PortRef
import           NodeEditor.Action.Basic                 (localMovePort, localRemovePort, setInputSidebarPortMode)
import qualified NodeEditor.Action.Basic                 as Basic
import qualified NodeEditor.Action.Batch                 as Batch
import           NodeEditor.Action.Command               (Command)
import qualified NodeEditor.Action.Connect               as Connect
import           NodeEditor.Action.State.Action          (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                          updateActionWithKey)
import           NodeEditor.Action.State.App             (renderIfNeeded)
import           NodeEditor.Action.State.NodeEditor      (getInputNode, modifyInputNode)
import           NodeEditor.Action.State.Scene           (getInputSidebarSize)
import           NodeEditor.Event.Mouse                  (mousePosition)
import           NodeEditor.React.Model.Constants        (gridSize)
import           NodeEditor.React.Model.Node.SidebarNode (NodeLoc, countProjectionPorts, outPortAt)
import           NodeEditor.React.Model.Port             (AnyPortRef (OutPortRef'), OutPortIndex (Projection), OutPortRef, getPortNumber)
import qualified NodeEditor.React.Model.Port             as Port
import           NodeEditor.React.Model.Sidebar          (portPositionInInputSidebar)
import qualified NodeEditor.React.View.Sidebar           as Sidebar
import           NodeEditor.State.Action                 (Action (begin, continue, end, update), Connect, Mode (Click, Drag),
                                                          PortDrag (PortDrag), connectIsPortPhantom, connectMode, connectSourcePort,
                                                          connectStartPos, portDragActPortRef, portDragAction, portDragIsPortPhantom,
                                                          portDragMode, portDragPortStartPosInSidebar, portDragStartPortRef,
                                                          portDragStartPos)
import           NodeEditor.State.Global                 (State)
import           React.Flux                              (MouseEvent)


instance Action (Command State) PortDrag where
    begin      = beginActionWithKey    portDragAction
    continue   = continueActionWithKey portDragAction
    update     = updateActionWithKey   portDragAction
    end action = if action ^. portDragIsPortPhantom
        then do
            void $ localRemovePort $ action ^. portDragActPortRef
            removeActionFromState portDragAction
        else cancelPortDragUnsafe action

addPort :: OutPortRef -> Command State ()
addPort portRef = Basic.addPort portRef Nothing

removePort :: OutPortRef -> Command State ()
removePort portRef = do
    -- freezeSidebar $ portRef ^. PortRef.nodeLoc
    Basic.removePort portRef

startPortNameEdit :: OutPortRef -> Command State ()
startPortNameEdit portRef = do
    let nodeLoc = portRef ^. PortRef.nodeLoc
        pid     = portRef ^. PortRef.srcPortId
    modifyInputNode nodeLoc $ outPortAt pid . Port.mode .= Port.NameEdit
    renderIfNeeded
    liftIO Sidebar.focusPortLabel

cancelPortNameEdit :: OutPortRef -> Command State ()
cancelPortNameEdit portRef = do
    let nodeLoc = portRef ^. PortRef.nodeLoc
        portId  = portRef ^. PortRef.srcPortId
    modifyInputNode nodeLoc $ outPortAt portId . Port.mode .= Port.Normal

finishPortNameEdit :: OutPortRef -> String -> Command State ()
finishPortNameEdit portRef name = do
    let nodeLoc = portRef ^. PortRef.nodeLoc
        portId  = portRef ^. PortRef.srcPortId
    Batch.renamePort portRef name
    modifyInputNode nodeLoc $ outPortAt portId %= (\p -> p & Port.name .~ name
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

startPortDrag :: ScreenPosition -> OutPortRef -> Bool -> Mode -> Command State ()
startPortDrag mousePos portRef isPhantom mode = do
    maySuccess <- runMaybeT $ do
        let portId  = portRef ^. PortRef.srcPortId
        portPos <- MaybeT $ fmap2 (flip portPositionInInputSidebar portId) getInputSidebarSize
        lift . setInputSidebarPortMode portRef $ Port.Moved portPos
        lift . begin $ PortDrag mousePos portPos portRef portRef isPhantom mode
    when (isNothing maySuccess && isPhantom) $ void $ localRemovePort portRef

handleMove :: MouseEvent -> PortDrag -> Command State ()
handleMove evt portDrag = do
    mousePos <- mousePosition evt
    let portRef       = portDrag ^. portDragActPortRef
        nodeLoc       = portRef  ^. PortRef.nodeLoc
        portId        = portRef  ^. PortRef.srcPortId
        startPortPos  = portDrag ^. portDragPortStartPosInSidebar
        startMousePos = portDrag ^. portDragStartPos
        newPos        = startPortPos & y +~ mousePos ^. y - startMousePos ^. y
    mayNewPortNum <- case portId of
        (Projection i: _) -> runMaybeT $ do
            node <- MaybeT $ getInputNode nodeLoc
            let newNum = min (countProjectionPorts node - 1) $ max 0 (round $ newPos ^. y / gridSize)
            if newNum /= i then return newNum else nothing
        _                          -> $notImplemented
    setInputSidebarPortMode portRef $ Port.Moved newPos
    withJust mayNewPortNum $ \newNum ->
        withJustM (localMovePort portRef newNum) $ \newPortRef ->
            update $ portDrag & portDragActPortRef .~ newPortRef

cancelPortDragUnsafe :: PortDrag -> Command State ()
cancelPortDragUnsafe portDrag = do
    let portRef    = portDrag ^. portDragActPortRef
        orgPortRef = portDrag ^. portDragStartPortRef
        orgPortId  = orgPortRef ^. PortRef.srcPortId
    setInputSidebarPortMode portRef Port.Normal
    when (portRef /= orgPortRef) $
        void $ localMovePort portRef $ getPortNumber orgPortId
    removeActionFromState portDragAction

finishPortDrag :: PortDrag -> Command State ()
finishPortDrag portDrag = do
    let portRef    = portDrag ^. portDragActPortRef
        orgPortRef = portDrag ^. portDragStartPortRef
        portId     = portRef ^. PortRef.srcPortId
        isPhantom  = portDrag ^. portDragIsPortPhantom
    if portRef == orgPortRef then end portDrag else do
        setInputSidebarPortMode portRef Port.Normal
        if isPhantom
            then Batch.addPort portRef Nothing
            else Batch.movePort orgPortRef $ getPortNumber portId
        removeActionFromState portDragAction

restoreConnect :: PortDrag -> Command State ()
restoreConnect portDrag = do
    cancelPortDragUnsafe portDrag
    Connect.startConnecting (portDrag ^. portDragStartPos) (OutPortRef' $ portDrag ^. portDragStartPortRef) Nothing (portDrag ^. portDragIsPortPhantom) (portDrag ^. portDragMode)

restorePortDrag :: NodeLoc -> Connect -> Command State ()
restorePortDrag nodeLoc connect = when (connect ^. connectSourcePort . PortRef.nodeLoc == nodeLoc) $ do
    Connect.stopConnectingUnsafe connect
    case connect ^. connectSourcePort of
        OutPortRef' sourcePort -> startPortDrag (connect ^. connectStartPos) sourcePort (connect ^. connectIsPortPhantom) (connect ^. connectMode)
        _                      -> return ()
