module Luna.Studio.Action.Basic.SetPortDefault where

import           Empire.API.Data.Port                (PortState (WithDefault))
import           Empire.API.Data.PortDefault         (PortDefault)
import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Data.PortRef            (AnyPortRef, nodeLoc, portId)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node        as Node
import qualified Luna.Studio.React.Model.Port        as Port
import           Luna.Studio.State.Global            (State)


setPortDefault :: AnyPortRef -> PortDefault -> Command State ()
setPortDefault portRef portDef = whenM (localSetPortDefault portRef portDef) $
    Batch.setPortDefault portRef portDef

localSetPortDefault :: AnyPortRef -> PortDefault -> Command State Bool
localSetPortDefault portRef portDef = do
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. portId
    NodeEditor.modifyExpressionNode nl $ Node.ports . ix pid . Port.state .= WithDefault portDef
    isJust <$> NodeEditor.getPort portRef
