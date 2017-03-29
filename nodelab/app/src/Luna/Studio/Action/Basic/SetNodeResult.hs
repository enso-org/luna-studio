module Luna.Studio.Action.Basic.SetNodeResult where

import           Empire.API.Graph.NodeResultUpdate           (NodeValue)
import           Luna.Studio.Action.Basic.DrawConnection     (redrawConnectionsForNode)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeLoc, execTime, value)
import           Luna.Studio.State.Global                    (State)


setNodeValue :: NodeLoc -> NodeValue -> Command State ()
setNodeValue nl val = do
    modifyExpressionNode nl $ value ?= val
    void $ redrawConnectionsForNode nl

setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t = do
    modifyExpressionNode nl $ execTime ?= t
    void $ redrawConnectionsForNode nl
