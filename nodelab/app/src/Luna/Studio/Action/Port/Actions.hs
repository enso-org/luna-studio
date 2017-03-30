module Luna.Studio.Action.Port.Actions
    ( handleMouseDown
    , handleClick
    ) where

import           Empire.API.Data.PortRef                  (AnyPortRef, nodeLoc, portId)
import           Luna.Studio.Action.Basic                 (localAddPort)
import           Luna.Studio.Action.Command               (Command)
import           Luna.Studio.Action.Connect               (connectToPort, startConnecting)
import           Luna.Studio.Action.Sidebar               (startPortDrag)
import           Luna.Studio.Event.Mouse                  (mousePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node             (Node (Sidebar), countProjectionPorts, hasPort)
import           Luna.Studio.React.Model.Node.SidebarNode (isOutputSidebar)
import           Luna.Studio.React.Model.Port             (getPortNumber)
import           Luna.Studio.State.Action                 (Action (continue), Mode (Click, Drag), connectAction, connectMode,
                                                           portDragAction, portDragMode)

import           Luna.Studio.Action.State.Action          (checkAction, checkIfActionPerfoming)
import           Luna.Studio.Action.State.NodeEditor      (getNode)
import           Luna.Studio.State.Global                 (State)
import           React.Flux                               (MouseEvent)


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
    mayNode <- getNode $ portRef ^. nodeLoc
    withJust mayNode $ \node -> do
        mousePos <- mousePosition evt
        let doConnect = case node of
                Sidebar n -> isOutputSidebar n
                _         -> True
        if doConnect
            then startConnecting mousePos portRef Nothing False mode
            else do
                isPhantom <- addPhantomPortIfPossibleAndNeeded portRef
                startPortDrag mousePos portRef isPhantom mode

addPhantomPortIfPossibleAndNeeded :: AnyPortRef -> Command State Bool
addPhantomPortIfPossibleAndNeeded portRef = do
    let nid = portRef ^. nodeLoc
        pid = portRef ^. portId
    mayNode <- getNode nid
    case mayNode of
        Nothing   -> return False
        Just node -> if hasPort pid node || countProjectionPorts node /= getPortNumber pid
            then return False
            else localAddPort portRef
