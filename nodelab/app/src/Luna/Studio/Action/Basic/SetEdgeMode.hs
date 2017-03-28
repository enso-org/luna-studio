module Luna.Studio.Action.Basic.SetEdgeMode where

import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.State.NodeEditor   (modifyEdgeNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.EdgeNode (EdgeMode (AddRemove, MoveConnect), NodeLoc, mode)
import           Luna.Studio.State.Global              (State)


setEdgeMode :: NodeLoc -> EdgeMode -> Command State ()
setEdgeMode nl edgeMode = modifyEdgeNode nl $ mode .= edgeMode

toggleEdgeMode :: NodeLoc -> Command State ()
toggleEdgeMode nl = modifyEdgeNode nl $ mode %= toggleMode where
    toggleMode (AddRemove)   = MoveConnect
    toggleMode (MoveConnect) = AddRemove
