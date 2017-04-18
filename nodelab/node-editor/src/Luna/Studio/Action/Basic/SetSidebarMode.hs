module Luna.Studio.Action.Basic.SetSidebarMode where

import           Luna.Studio.Action.Command               (Command)
import           Luna.Studio.Action.State.NodeEditor      (modifyInputNode, modifyOutputNode)
import           Luna.Prelude
import           Luna.Studio.React.Model.Node.SidebarNode (NodeLoc, SidebarMode (AddRemove, MoveConnect), inputMode, outputMode)
import           Luna.Studio.State.Global                 (State)


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
