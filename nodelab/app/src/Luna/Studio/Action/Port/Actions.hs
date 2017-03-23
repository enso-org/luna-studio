module Luna.Studio.Action.Port.Actions
    ( handleMouseDown
    , handleClick
    ) where

import           Empire.API.Data.PortRef               (AnyPortRef)
import qualified Empire.API.Data.PortRef               as PortRef
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.Connect            (connectToPort, startConnecting)
import           Luna.Studio.Action.Edge               (startPortDrag)
import           Luna.Studio.Event.Mouse               (mousePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node          (Node (Edge))
import           Luna.Studio.React.Model.Node.EdgeNode (isInputEdge)
import           Luna.Studio.State.Action              (Action (continue), Mode (Click, Drag), connectAction, connectMode, portDragAction)

import           Luna.Studio.Action.State.Action       (checkAction)
import           Luna.Studio.Action.State.NodeEditor   (getNode)
import           Luna.Studio.State.Global              (State)
import           React.Flux                            (MouseEvent)


handleMouseDown :: MouseEvent -> AnyPortRef -> Command State ()
handleMouseDown evt portRef = do
    mayConnect  <- checkAction connectAction
    mayPortDrag <- checkAction portDragAction
    when ( Just Click /= (view connectMode <$> mayConnect)
        && Just Click /= (view connectMode <$> mayPortDrag) ) $
        startPortDragOrConnect evt portRef Drag

handleClick :: MouseEvent -> AnyPortRef -> Command State ()
handleClick evt portRef = do
    mayConnect <- checkAction connectAction
    if (Just Click == (view connectMode <$> mayConnect)) then
            continue $ connectToPort portRef
        else startPortDragOrConnect evt portRef Click

startPortDragOrConnect :: MouseEvent -> AnyPortRef -> Mode -> Command State ()
startPortDragOrConnect evt portRef mode = do
    mayNode <- getNode $ portRef ^. PortRef.nodeId
    withJust mayNode $ \node -> do
        mousePos <- mousePosition evt
        let doPortDrag = case node of
                Edge n -> isInputEdge n
                _      -> False
        if False --doPortDrag
        then startPortDrag   mousePos portRef         mode
        else startConnecting mousePos portRef Nothing mode
