module NodeEditor.Action.Basic.SetPortMode where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           LunaStudio.Data.PortRef            (InPortRef, OutPortRef, dstPortId, nodeLoc, srcPortId)
import           NodeEditor.Action.State.NodeEditor (modifyInputNode, modifyOutputNode)
import           NodeEditor.React.Model.Node        (inPortAt, outPortAt)
import           NodeEditor.React.Model.Port        (Mode, mode)
import           NodeEditor.State.Global            (State)


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
