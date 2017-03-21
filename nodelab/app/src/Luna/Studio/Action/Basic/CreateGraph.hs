module Luna.Studio.Action.Basic.CreateGraph where

import           Empire.API.Data.MonadPath              (MonadPath)
import           Empire.API.Data.PortRef                (InPortRef, OutPortRef)
import           Luna.Studio.Action.Basic.AddConnection (localAddConnections)
import           Luna.Studio.Action.Basic.AddNode       (localAddAnyNode)
import           Luna.Studio.Action.Basic.FocusNode     (updateNodeZOrder)
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.State.NodeEditor    (updateMonads)
import qualified Luna.Studio.Action.State.NodeEditor    as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.EdgeNode       (EdgeNode)
import           Luna.Studio.React.Model.Node           (Node)
import           Luna.Studio.State.Global               (State)


createGraph :: [Either Node EdgeNode] -> [(OutPortRef, InPortRef)] -> [MonadPath] -> Command State ()
createGraph nodes connections monads = do
    NodeEditor.resetGraph
    mapM_ localAddAnyNode nodes
    void $ localAddConnections connections
    updateMonads monads
    updateNodeZOrder
