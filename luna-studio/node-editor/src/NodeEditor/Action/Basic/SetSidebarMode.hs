module NodeEditor.Action.Basic.SetSidebarMode where

import           Common.Action.Command                   (Command)
import           Common.Prelude
import           NodeEditor.Action.State.NodeEditor      (modifyInputNode, modifyOutputNode)
import           NodeEditor.React.Model.Node.SidebarNode (NodeLoc, SidebarMode (AddRemove, MoveConnect), inputMode, outputMode)
import           NodeEditor.State.Global                 (State)


setInputMode :: NodeLoc -> SidebarMode -> Command State ()
setInputMode nl newMode = modifyInputNode nl $ inputMode .= newMode

toggleInputMode :: NodeLoc -> Command State ()
toggleInputMode nl = modifyInputNode nl $ inputMode %= toggle

setOutputMode :: NodeLoc -> SidebarMode -> Command State ()
setOutputMode nl newMode = modifyOutputNode nl $ outputMode .= newMode

toggleOutputMode :: NodeLoc -> Command State ()
toggleOutputMode nl = modifyOutputNode nl $ outputMode %= toggle

toggle :: SidebarMode -> SidebarMode
toggle AddRemove   = MoveConnect
toggle MoveConnect = AddRemove
