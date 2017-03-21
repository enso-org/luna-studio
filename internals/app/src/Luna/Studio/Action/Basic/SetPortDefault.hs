module Luna.Studio.Action.Basic.SetPortDefault where

import qualified Empire.API.Data.Node                as Node
import           Empire.API.Data.Port                (PortState (WithDefault))
import qualified Empire.API.Data.Port                as Port
import           Empire.API.Data.PortDefault         (PortDefault)
import           Empire.API.Data.PortRef             (AnyPortRef, nodeId, portId)
import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import qualified Luna.Studio.Action.State.Graph      as Graph
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node        as Model
import qualified Luna.Studio.React.Model.Port        as Model
import           Luna.Studio.State.Global            (State)


setPortDefault :: AnyPortRef -> PortDefault -> Command State ()
setPortDefault portRef portDef = whenM (localSetPortDefault portRef portDef) $
    Batch.setPortDefault portRef portDef

localSetPortDefault :: AnyPortRef -> PortDefault -> Command State Bool
localSetPortDefault portRef portDef = do
    let nid = portRef ^. nodeId
        pid = portRef ^. portId
    NodeEditor.modifyNode nid $ Model.ports . ix pid . Model.state .= WithDefault portDef
    Graph.modifyNode      nid $ Node.ports . ix pid . Port.state .~ WithDefault portDef
    isJust <$> NodeEditor.getPort portRef
