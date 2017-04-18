module Luna.Studio.Action.Basic.CreateGraph where

import           Empire.API.Data.MonadPath              (MonadPath)
import           Empire.API.Data.PortRef                (InPortRef, OutPortRef)
import           Luna.Studio.Action.Basic.AddConnection (localAddConnections)
import           Luna.Studio.Action.Basic.AddNode       (localAddExpressionNodes)
import           Luna.Studio.Action.Basic.FocusNode     (updateNodeZOrder)
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.State.NodeEditor    (addInputNode, addOutputNode, updateMonads)
import qualified Luna.Studio.Action.State.NodeEditor    as NodeEditor
import           Luna.Prelude
import           Luna.Studio.React.Model.Node           (ExpressionNode, InputNode, OutputNode)
import           Luna.Studio.State.Global               (State)


createGraph :: [ExpressionNode] -> Maybe InputNode -> Maybe OutputNode -> [(OutPortRef, InPortRef)] -> [MonadPath] -> Command State ()
createGraph nodes input output connections monads = do
    NodeEditor.resetGraph
    localAddExpressionNodes nodes
    mapM_ addInputNode input
    mapM_ addOutputNode output
    void $ localAddConnections connections
    updateMonads monads
    updateNodeZOrder
