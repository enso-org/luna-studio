module Luna.Studio.Action.Basic.AddPort where

import           Empire.API.Data.Port                   (Port (Port))
import           Empire.API.Data.PortRef                (AnyPortRef (OutPortRef'), OutPortRef (OutPortRef), srcPortId)
import           Empire.API.Data.TypeRep                (TypeRep (TStar))
import           Luna.Studio.Action.Basic.AddConnection (localAddConnection)
import           Luna.Studio.Action.Basic.UpdateNode    (localUpdateEdgeNode)
import qualified Luna.Studio.Action.Batch               as Batch
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.State.NodeEditor    (getConnectionsContainingNode, getEdgeNode)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Connection     as Connection
import           Luna.Studio.React.Model.Node.EdgeNode  (countProjectionPorts, getPorts, isInputEdge, ports)
import           Luna.Studio.React.Model.Port           (OutPort (Projection), PortId (OutPortId), PortState (NotConnected), portId,
                                                         toPortsMap)
import           Luna.Studio.State.Global               (State)


addPort :: AnyPortRef -> Command State ()
addPort portRef = whenM (localAddPort portRef) $ Batch.addPort portRef

localAddPort :: AnyPortRef -> Command State Bool
localAddPort (OutPortRef' (OutPortRef nid pid@(Projection pos))) = do
    mayNode <- getEdgeNode nid
    flip (maybe (return False)) mayNode $ \node ->
        if     (not . isInputEdge $ node)
            || pos > countProjectionPorts node
            || pos < 0
            then return False
            else do
                let newPort     = convert $ Port (OutPortId pid) "" TStar NotConnected
                    oldPorts    = getPorts node
                    newPorts'   = flip map oldPorts $ \port' -> case port' ^. portId of
                        OutPortId (Projection i) ->
                            if i < pos
                                then port'
                                else port' & portId .~ (OutPortId $ Projection (i+1))
                        _                        -> port'
                    newPorts    = newPort : newPorts'
                    newPortsMap = toPortsMap newPorts
                void . localUpdateEdgeNode $ node & ports .~ newPortsMap
                conns <- getConnectionsContainingNode nid
                forM_ conns $ \conn -> case conn ^. Connection.src of
                    (OutPortRef srcNid (Projection i)) ->
                        when (srcNid == nid && i >= pos) $
                            void $ localAddConnection (conn ^. Connection.src & srcPortId .~ Projection (i+1)) (conn ^. Connection.dst)
                    _ -> return ()
                return True
localAddPort _ = $notImplemented