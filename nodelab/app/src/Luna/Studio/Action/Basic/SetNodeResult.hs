module Luna.Studio.Action.Basic.SetNodeResult where

import           Empire.API.Data.Node                        (NodeId)
import           Empire.API.Graph.NodeResultUpdate           (NodeValue)
import           Luna.Studio.Action.Basic.DrawConnection     (redrawConnectionsForNode)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (execTime, value)
import           Luna.Studio.State.Global                    (State)


setNodeValue :: NodeId -> NodeValue -> Command State ()
setNodeValue nid val = do
    modifyExpressionNode nid $ value ?= val
    void $ redrawConnectionsForNode nid

setNodeProfilingData :: NodeId -> Integer -> Command State ()
setNodeProfilingData nid t = do
    modifyExpressionNode nid $ execTime ?= t
    void $ redrawConnectionsForNode nid
