module Node.Editor.Action.Basic.SetNodeResult where

import           Empire.API.Graph.NodeResultUpdate           (NodeValue)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.State.NodeEditor         (modifyExpressionNode)
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (NodeLoc, execTime, value)
import           Node.Editor.State.Global                    (State)


setNodeValue :: NodeLoc -> NodeValue -> Command State ()
setNodeValue nl val = modifyExpressionNode nl $ value ?= val

setNodeProfilingData :: NodeLoc -> Integer -> Command State ()
setNodeProfilingData nl t = modifyExpressionNode nl $ execTime ?= t
