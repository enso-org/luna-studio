module Luna.Studio.Action.Basic.SetEdgeMode where

import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.State.NodeEditor   (modifyEdgeNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.EdgeNode (EdgeMode (AddRemove, MoveConnect), NodeId, mode)
import           Luna.Studio.State.Global              (State)


setEdgeMode :: NodeId -> EdgeMode -> Command State ()
setEdgeMode nid edgeMode = modifyEdgeNode nid $ mode .= edgeMode

toggleEdgeMode :: NodeId -> Command State ()
toggleEdgeMode nid = modifyEdgeNode nid $ mode %= toggleMode where
    toggleMode (AddRemove)   = MoveConnect
    toggleMode (MoveConnect) = AddRemove
