module Node.Editor.Action.Basic.SetPortDefault where

import           Empire.API.Data.Port                (PortState (WithDefault))
import           Empire.API.Data.PortDefault         (PortDefault)
import           Empire.API.Data.PortRef             (InPortRef, dstPortId, nodeLoc)
import qualified Node.Editor.Action.Batch            as Batch
import           Node.Editor.Action.Command          (Command)
import qualified Node.Editor.Action.State.NodeEditor as NodeEditor
import           Luna.Prelude
import qualified Node.Editor.React.Model.Node        as Node
import           Node.Editor.React.Model.NodeEditor  (getPort)
import qualified Node.Editor.React.Model.Port        as Port
import           Node.Editor.State.Global            (State)


setPortDefault :: InPortRef -> PortDefault -> Command State ()
setPortDefault portRef portDef = whenM (localSetPortDefault portRef portDef) $
    Batch.setPortDefault portRef portDef

localSetPortDefault :: InPortRef -> PortDefault -> Command State Bool
localSetPortDefault portRef portDef = do
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. dstPortId
    NodeEditor.modifyExpressionNode nl $ Node.inPortAt pid . Port.state .= WithDefault portDef
    isJust <$> (getPort portRef <$> NodeEditor.getNodeEditor)
