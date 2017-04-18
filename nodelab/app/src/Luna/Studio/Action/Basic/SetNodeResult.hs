module Luna.Studio.Action.Basic.SetNodeResult where

import           Empire.API.Graph.NodeResultUpdate           (NodeValue)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (NodeLoc, execTime, value)
import           Luna.Studio.State.Global                    (State)


setNodeValue :: NodeLoc -> NodeValue -> Command State ()
setNodeValue nl val = modifyExpressionNode nl $ value ?= val

setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t = modifyExpressionNode nl $ execTime ?= t
