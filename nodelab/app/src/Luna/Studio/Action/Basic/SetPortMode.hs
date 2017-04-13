module Luna.Studio.Action.Basic.SetPortMode where

import           Empire.API.Data.PortRef                 (InPortRef, OutPortRef, dstPortId, nodeLoc, srcPortId)
import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNode)
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.State.NodeEditor     (modifyInputNode, modifyOutputNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node            (inPortAt, outPortAt)
import           Luna.Studio.React.Model.Port            (Mode, mode)
import           Luna.Studio.State.Global                (State)


setInputSidebarPortMode :: OutPortRef -> Mode -> Command State ()
setInputSidebarPortMode portRef m = do
    let nl = portRef ^. nodeLoc
        pid = portRef ^. srcPortId
    modifyInputNode  nl $ outPortAt pid . mode .= m
    void $ redrawConnectionsForNode nl


setOutputSidebarPortMode :: InPortRef -> Mode -> Command State ()
setOutputSidebarPortMode portRef m = do
    let nl = portRef ^. nodeLoc
        pid = portRef ^. dstPortId
    modifyOutputNode nl $ inPortAt pid . mode .= m
    void $ redrawConnectionsForNode nl
