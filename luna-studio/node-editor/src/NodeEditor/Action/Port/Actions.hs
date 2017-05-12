module NodeEditor.Action.Port.Actions
    ( handleMouseDown
    , handleClick
    ) where

import           Common.Prelude
import           LunaStudio.Data.PortRef            (AnyPortRef (OutPortRef'), OutPortRef, nodeLoc, srcPortId)
import           NodeEditor.Action.Basic            (localAddPort)
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.Action.Connect          (connectToPort, startConnecting)
import           NodeEditor.Action.Sidebar          (startPortDrag)
import           NodeEditor.Event.Mouse             (mousePosition)
import           NodeEditor.React.Model.Node        (countProjectionPorts, hasPort)
import           NodeEditor.React.Model.Port        (getPortNumber)
import           NodeEditor.State.Action            (Action (continue), Mode (Click, Drag), connectAction, connectMode, portDragAction,
                                                     portDragMode)

import           NodeEditor.Action.State.Action     (checkAction, checkIfActionPerfoming)
import           NodeEditor.Action.State.NodeEditor (getInputNode)
import           NodeEditor.State.Global            (State)
import           React.Flux                         (MouseEvent)


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
            else localAddPort portRef Nothing
