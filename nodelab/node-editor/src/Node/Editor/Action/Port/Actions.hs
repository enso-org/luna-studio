module Node.Editor.Action.Port.Actions
    ( handleMouseDown
    , handleClick
    ) where

import           Empire.API.Data.PortRef             (AnyPortRef (OutPortRef'), OutPortRef, nodeLoc, srcPortId)
import           Node.Editor.Action.Basic            (localAddPort)
import           Node.Editor.Action.Command          (Command)
import           Node.Editor.Action.Connect          (connectToPort, startConnecting)
import           Node.Editor.Action.Sidebar          (startPortDrag)
import           Node.Editor.Event.Mouse             (mousePosition)
import           Luna.Prelude
import           Node.Editor.React.Model.Node        (countProjectionPorts, hasPort)
import           Node.Editor.React.Model.Port        (getPortNumber)
import           Node.Editor.State.Action            (Action (continue), Mode (Click, Drag), connectAction, connectMode, portDragAction,
                                                      portDragMode)

import           Node.Editor.Action.State.Action     (checkAction, checkIfActionPerfoming)
import           Node.Editor.Action.State.NodeEditor (getInputNode)
import           Node.Editor.State.Global            (State)
import           React.Flux                          (MouseEvent)


handleMouseDown :: MouseEvent -> AnyPortRef -> Command State ()
handleMouseDown evt portRef = do
    mayConnect  <- checkAction connectAction
    mayPortDrag <- checkAction portDragAction
    when ( Just Click /= (view connectMode  <$> mayConnect)
        && Just Click /= (view portDragMode <$> mayPortDrag) ) $
        startPortDragOrConnect evt portRef Drag

handleClick :: MouseEvent -> AnyPortRef -> Command State ()
handleClick evt portRef = do
    mayConnect <- checkAction connectAction
    newAction  <- not <$> checkIfActionPerfoming portDragAction
    if Just Click == (view connectMode <$> mayConnect) then continue $ connectToPort portRef
    else if newAction                                  then startPortDragOrConnect evt portRef Click
    else return ()

startPortDragOrConnect :: MouseEvent -> AnyPortRef -> Mode -> Command State ()
startPortDragOrConnect evt portRef mode = do
    mousePos     <- mousePosition evt
    mayInputNode <- getInputNode (portRef ^. nodeLoc)
    case (mayInputNode, portRef) of
        (Just _, OutPortRef' inPortRef) -> do
            isPhantom <- addPhantomPortIfPossibleAndNeeded inPortRef
            startPortDrag mousePos inPortRef isPhantom mode
        _ -> startConnecting mousePos portRef Nothing False mode

addPhantomPortIfPossibleAndNeeded :: OutPortRef -> Command State Bool
addPhantomPortIfPossibleAndNeeded portRef = do
    let nid = portRef ^. nodeLoc
        pid = portRef ^. srcPortId
    getInputNode nid >>= \case
        Nothing   -> return False
        Just node -> if hasPort pid node || countProjectionPorts node /= getPortNumber pid
            then return False
            else localAddPort portRef
