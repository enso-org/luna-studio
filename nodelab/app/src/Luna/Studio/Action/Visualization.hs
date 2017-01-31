module Luna.Studio.Action.Visualization where

import           Empire.API.Data.Node               (NodeId)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global



pin :: NodeId -> Int -> Command State ()
pin nodeId visIx = do
    mayNode <- Global.getNode nodeId
    withJust mayNode $ \node ->
        Global.modifyNodeEditor $
            NodeEditor.visualizations . at (nodeId, visIx) ?= node ^. Node.position

unpin :: NodeId -> Int -> Command State ()
unpin nodeId visIx =
    Global.modifyNodeEditor $
        NodeEditor.visualizations . at (nodeId, visIx) .= def
