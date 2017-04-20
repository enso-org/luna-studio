module NodeEditor.Action.Basic.CreateGraph where

import           Empire.API.Data.MonadPath              (MonadPath)
import           Empire.API.Data.PortRef                (InPortRef, OutPortRef)
import           NodeEditor.Action.Basic.AddConnection (localAddConnections)
import           NodeEditor.Action.Basic.AddNode       (localAddExpressionNodes)
import           NodeEditor.Action.Basic.FocusNode     (updateNodeZOrder)
import           NodeEditor.Action.Command             (Command)
import           NodeEditor.Action.State.NodeEditor    (addInputNode, addOutputNode, updateMonads)
import qualified NodeEditor.Action.State.NodeEditor    as NodeEditor
import           Common.Prelude
import           NodeEditor.React.Model.Node           (ExpressionNode, InputNode, OutputNode)
import           NodeEditor.State.Global               (State)


createGraph :: [ExpressionNode] -> Maybe InputNode -> Maybe OutputNode -> [(OutPortRef, InPortRef)] -> [MonadPath] -> Command State ()
createGraph nodes input output connections monads = do
    NodeEditor.resetGraph
    localAddExpressionNodes nodes
    mapM_ addInputNode input
    mapM_ addOutputNode output
    void $ localAddConnections connections
    updateMonads monads
    updateNodeZOrder
