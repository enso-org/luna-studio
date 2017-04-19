module Node.Editor.Action.Basic.SetPortMode where

import           Empire.API.Data.PortRef             (InPortRef, OutPortRef, dstPortId, nodeLoc, srcPortId)
import           Node.Editor.Action.Command          (Command)
import           Node.Editor.Action.State.NodeEditor (modifyInputNode, modifyOutputNode)
import           Luna.Prelude
import           Node.Editor.React.Model.Node        (inPortAt, outPortAt)
import           Node.Editor.React.Model.Port        (Mode, mode)
import           Node.Editor.State.Global            (State)


setInputSidebarPortMode :: OutPortRef -> Mode -> Command State ()
setInputSidebarPortMode portRef m = do
    let nl = portRef ^. nodeLoc
        pid = portRef ^. srcPortId
    modifyInputNode  nl $ outPortAt pid . mode .= m

setOutputSidebarPortMode :: InPortRef -> Mode -> Command State ()
setOutputSidebarPortMode portRef m = do
    let nl = portRef ^. nodeLoc
        pid = portRef ^. dstPortId
    modifyOutputNode nl $ inPortAt pid . mode .= m
