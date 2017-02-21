module Luna.Studio.Action.Port.Actions
    ( handleMouseDown
    , handleClick
    ) where

import           Empire.API.Data.PortRef      (AnyPortRef)
import qualified Empire.API.Data.PortRef      as PortRef
import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Action.Connect   (connectToPort, startConnecting)
import           Luna.Studio.Action.Edge      (startPortDrag)
import           Luna.Studio.Event.Mouse      (mousePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node (isInputEdge)
import           Luna.Studio.State.Action     (Action (continue), Mode (Click, Drag), connectAction, portDragAction)
import qualified Luna.Studio.State.Action     as Action
import           Luna.Studio.State.Global     (State, checkAction)
import qualified Luna.Studio.State.Global     as Global
import           React.Flux                   (MouseEvent)


handleMouseDown :: MouseEvent -> AnyPortRef -> Command State ()
handleMouseDown evt portRef = do
    mayConnect  <- checkAction connectAction
    mayPortDrag <- checkAction portDragAction
    when ( Just Click /= (view Action.connectMode <$> mayConnect)
        && Just Click /= (view Action.connectMode <$> mayPortDrag) ) $
        startPortDragOrConnect evt portRef Drag

handleClick :: MouseEvent -> AnyPortRef -> Command State ()
handleClick evt portRef = do
    mayConnect <- checkAction connectAction
    if (Just Click == (view Action.connectMode <$> mayConnect)) then
            continue $ connectToPort portRef
        else startPortDragOrConnect evt portRef Click

startPortDragOrConnect :: MouseEvent -> AnyPortRef -> Mode -> Command State ()
startPortDragOrConnect evt portRef mode = do
    mayNode <- Global.getNode $ portRef ^. PortRef.nodeId
    withJust mayNode $ \node -> if (isInputEdge node) then
            startPortDrag evt portRef mode
        else do
            mousePos <- mousePosition evt
            startConnecting mousePos portRef Nothing mode
