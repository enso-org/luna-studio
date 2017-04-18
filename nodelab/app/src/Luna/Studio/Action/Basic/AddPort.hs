module Luna.Studio.Action.Basic.AddPort where

import           Empire.API.Data.LabeledTree              (LabeledTree (LabeledTree))
import           Empire.API.Data.Port                     (Port (Port))
import           Empire.API.Data.PortRef                  (OutPortRef (OutPortRef), srcPortId)
import           Empire.API.Data.TypeRep                  (TypeRep (TStar))
import           Luna.Studio.Action.Basic.AddConnection   (localAddConnection)
import           Luna.Studio.Action.Basic.UpdateNode      (localUpdateInputNode)
import qualified Luna.Studio.Action.Batch                 as Batch
import           Luna.Studio.Action.Command               (Command)
import           Luna.Studio.Action.State.NodeEditor      (getConnectionsContainingNode, getInputNode)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Connection       as Connection
import           Luna.Studio.React.Model.Node.SidebarNode (countProjectionPorts, inputSidebarPorts)
import           Luna.Studio.React.Model.Port             (OutPortIndex (Projection), OutPorts (OutPorts), PortState (NotConnected))
import           Luna.Studio.State.Global                 (State)


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
