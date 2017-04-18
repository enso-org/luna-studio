module Luna.Studio.Action.Basic.SetPortDefault where

import           Empire.API.Data.Port                (PortState (WithDefault))
import           Empire.API.Data.PortDefault         (PortDefault)
import           Empire.API.Data.PortRef             (InPortRef, dstPortId, nodeLoc)
import qualified Luna.Studio.Action.Batch            as Batch
import           Luna.Studio.Action.Command          (Command)
import qualified Luna.Studio.Action.State.NodeEditor as NodeEditor
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node        as Node
import           Luna.Studio.React.Model.NodeEditor  (getPort)
import qualified Luna.Studio.React.Model.Port        as Port
import           Luna.Studio.State.Global            (State)


setPortDefault :: InPortRef -> PortDefault -> Command State ()
setPortDefault portRef portDef = whenM (localSetPortDefault portRef portDef) $
    Batch.setPortDefault portRef portDef

localSetPortDefault :: InPortRef -> PortDefault -> Command State Bool
localSetPortDefault portRef portDef = do
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. dstPortId
    NodeEditor.modifyExpressionNode nl $ Node.inPortAt pid . Port.state .= WithDefault portDef
    isJust <$> (getPort portRef <$> NodeEditor.getNodeEditor)
