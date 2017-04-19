module Node.Editor.Action.Basic.CreateGraph where

import           Empire.API.Data.MonadPath              (MonadPath)
import           Empire.API.Data.PortRef                (InPortRef, OutPortRef)
import           Node.Editor.Action.Basic.AddConnection (localAddConnections)
import           Node.Editor.Action.Basic.AddNode       (localAddExpressionNodes)
import           Node.Editor.Action.Basic.FocusNode     (updateNodeZOrder)
import           Node.Editor.Action.Command             (Command)
import           Node.Editor.Action.State.NodeEditor    (addInputNode, addOutputNode, updateMonads)
import qualified Node.Editor.Action.State.NodeEditor    as NodeEditor
import           Luna.Prelude
import           Node.Editor.React.Model.Node           (ExpressionNode, InputNode, OutputNode)
import           Node.Editor.State.Global               (State)


createGraph :: [ExpressionNode] -> Maybe InputNode -> Maybe OutputNode -> [(OutPortRef, InPortRef)] -> [MonadPath] -> Command State ()
createGraph nodes input output connections monads = do
    NodeEditor.resetGraph
    localAddExpressionNodes nodes
    mapM_ addInputNode input
    mapM_ addOutputNode output
    void $ localAddConnections connections
    updateMonads monads
    updateNodeZOrder
