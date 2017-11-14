module NodeEditor.Action.Basic.SetPortDefault where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           LunaStudio.Data.Port               (PortState (WithDefault), _WithDefault)
import           LunaStudio.Data.PortDefault        (PortDefault)
import           LunaStudio.Data.PortRef            (InPortRef, dstPortId, nodeLoc)
import qualified NodeEditor.Action.Batch            as Batch
import qualified NodeEditor.Action.State.NodeEditor as NodeEditor
import qualified NodeEditor.React.Model.Node        as Node
import           NodeEditor.React.Model.NodeEditor  (getPort)
import qualified NodeEditor.React.Model.Port        as Port
import           NodeEditor.State.Global            (State)


setPortDefault :: InPortRef -> PortDefault -> Command State ()
setPortDefault portRef portDef = whenM (localSetPortDefault portRef portDef) $
    Batch.setPortDefault portRef portDef

localSetPortDefault :: InPortRef -> PortDefault -> Command State Bool
localSetPortDefault portRef portDef = do
    let nl  = portRef ^. nodeLoc
        pid = portRef ^. dstPortId
    mayPrevVal <- maybe def (^? Node.inPortAt pid . Port.state . _WithDefault) <$> NodeEditor.getExpressionNode nl
    if mayPrevVal == Just portDef then return False else do
        NodeEditor.modifyExpressionNode nl $ Node.inPortAt pid . Port.state .= WithDefault portDef
        isJust <$> (getPort portRef <$> NodeEditor.getNodeEditor)
