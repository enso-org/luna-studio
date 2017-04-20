module NodeEditor.Action.Basic.AddPort where

import           Empire.API.Data.LabeledTree              (LabeledTree (LabeledTree))
import           Empire.API.Data.Port                     (Port (Port))
import           Empire.API.Data.PortRef                  (OutPortRef (OutPortRef), srcPortId)
import           Empire.API.Data.TypeRep                  (TypeRep (TStar))
import           NodeEditor.Action.Basic.AddConnection   (localAddConnection)
import           NodeEditor.Action.Basic.UpdateNode      (localUpdateInputNode)
import qualified NodeEditor.Action.Batch                 as Batch
import           NodeEditor.Action.Command               (Command)
import           NodeEditor.Action.State.NodeEditor      (getConnectionsContainingNode, getInputNode)
import           Common.Prelude
import qualified NodeEditor.React.Model.Connection       as Connection
import           NodeEditor.React.Model.Node.SidebarNode (countProjectionPorts, inputSidebarPorts)
import           NodeEditor.React.Model.Port             (OutPortIndex (Projection), OutPorts (OutPorts), PortState (NotConnected))
import           NodeEditor.State.Global                 (State)


addPort :: OutPortRef -> Command State ()
addPort portRef = whenM (localAddPort portRef) $ Batch.addPort portRef

localAddPort :: OutPortRef -> Command State Bool
localAddPort (OutPortRef nid pid@[Projection pos]) = do
    mayNode <- getInputNode nid
    flip (maybe (return False)) mayNode $ \node ->
        if pos > countProjectionPorts node
        || pos < 0
            then return False
            else do
                let newPort     = LabeledTree (OutPorts []) $ convert $ Port pid "" TStar NotConnected
                    oldPorts    = node ^. inputSidebarPorts
                    (portsBefore, portsAfter) = splitAt pos oldPorts
                    newPorts    = portsBefore <> [newPort] <> portsAfter
                void . localUpdateInputNode $ node & inputSidebarPorts .~ newPorts
                conns <- getConnectionsContainingNode nid
                forM_ conns $ \conn -> case conn ^. Connection.src of
                    (OutPortRef srcNid ((Projection i):p)) ->
                        when (srcNid == nid && i >= pos) $
                            void $ localAddConnection (conn ^. Connection.src & srcPortId .~ (Projection (i+1):p)) (conn ^. Connection.dst)
                    _ -> return ()
                return True
