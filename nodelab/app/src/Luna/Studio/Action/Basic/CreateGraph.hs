module Luna.Studio.Action.Basic.CreateGraph where

import           Empire.API.Data.MonadPath              (MonadPath)
import           Empire.API.Data.PortRef                (InPortRef, OutPortRef)
import           Luna.Studio.Action.Basic.AddConnection (localConnect)
import           Luna.Studio.Action.Basic.AddNode       (localAddNode)
import           Luna.Studio.Action.Basic.FocusNode     (updateNodeZOrder)
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.State.NodeEditor    (updateMonads)
import qualified Luna.Studio.Action.State.NodeEditor    as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node           (Node)
import           Luna.Studio.State.Global               (State)
import qualified Luna.Studio.State.Global               as Global


createGraph :: [Node] -> [(OutPortRef, InPortRef)] -> [MonadPath] -> Command State ()
createGraph nodes connections monads = do
    Global.graph .= def
    NodeEditor.resetGraph
    mapM_ localAddNode nodes
    mapM_ (uncurry localConnect) connections
    updateMonads monads
    updateNodeZOrder
