module Luna.Studio.Action.Basic.SetSidebarMode where

import           Luna.Studio.Action.Command               (Command)
import           Luna.Studio.Action.State.NodeEditor      (modifySidebarNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.SidebarNode (NodeLoc, SidebarMode (AddRemove, MoveConnect), mode)
import           Luna.Studio.State.Global                 (State)


setSidebarMode :: NodeLoc -> SidebarMode -> Command State ()
setSidebarMode nl sidebarMode = modifySidebarNode nl $ mode .= sidebarMode

toggleSidebarMode :: NodeLoc -> Command State ()
toggleSidebarMode nl = modifySidebarNode nl $ mode %= toggleMode where
    toggleMode (AddRemove)   = MoveConnect
    toggleMode (MoveConnect) = AddRemove
