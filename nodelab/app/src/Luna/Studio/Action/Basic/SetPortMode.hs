module Luna.Studio.Action.Basic.SetPortMode where

import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNode)
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.State.NodeEditor     (modifyEdgeNode)
import           Luna.Studio.Data.PortRef                (AnyPortRef, nodeLoc, portId)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node            (ports)
import           Luna.Studio.React.Model.Port            (Mode, mode)
import           Luna.Studio.State.Global                (State)


setEdgePortMode :: AnyPortRef -> Mode -> Command State ()
setEdgePortMode portRef m = do
    let nl = portRef ^. nodeLoc
        pid = portRef ^. portId
    modifyEdgeNode nl $ ports . ix pid . mode .= m
    void $ redrawConnectionsForNode nl
