module Luna.Studio.Action.Graph.AddPort where

import           Control.Arrow
import qualified Data.Map.Lazy                    as Map
import           Empire.API.Data.Connection       (Connection (Connection))
import qualified Empire.API.Data.Connection       as Connection
import           Empire.API.Data.Node             (NodeId, NodeType (InputEdge, OutputEdge))
import qualified Empire.API.Data.Node             as Node
import           Empire.API.Data.Port             (OutPort (All, Projection), Port (Port), PortId (InPortId, OutPortId),
                                                   PortState (NotConnected))
import qualified Empire.API.Data.Port             as Port
import           Empire.API.Data.PortRef          (AnyPortRef (OutPortRef'), OutPortRef (OutPortRef))
import qualified Empire.API.Data.PortRef          as PortRef
import           Empire.API.Data.TypeRep          (TypeRep (TStar))
import qualified Luna.Studio.Action.Batch         as Batch
import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Action.Graph.AddNode (localUpdateNode)
import           Luna.Studio.Action.Graph.Connect (localAddConnection)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global         (State)
import qualified Luna.Studio.State.Global         as Global
import           Luna.Studio.State.Graph          (connectionsContainingNode)
import qualified Luna.Studio.State.Graph          as Graph


addPort :: AnyPortRef -> Command State ()
addPort portRef = whenM (localAddPort portRef) $ Batch.addPort portRef

--TODO[LJK, PM, JK]: If this action returns False we should notify user about it since
-- GUI will looks like everything was send to backend but nothing happens
localAddPort :: AnyPortRef -> Command State Bool
localAddPort portRef = do
    let nodeId = portRef ^. PortRef.nodeId
        portId = portRef ^. PortRef.portId
    mayNode <- use $ Global.graph . Graph.nodesMap . at nodeId
    case mayNode of
        Nothing   -> return False
        Just node -> case (node ^. Node.nodeType, portId) of
            (OutputEdge, InPortId  _   )                   -> $notImplemented
            (InputEdge,  OutPortId All)                    -> $notImplemented
            (InputEdge,  pid@(OutPortId (Projection pos))) -> do
                let newPort     = Port pid "" TStar NotConnected
                    oldPorts    = node ^. Node.ports . to Map.elems
                    newPorts'   = flip map oldPorts $ \port -> case port ^. Port.portId of
                        OutPortId (Projection i) -> if i < pos then
                                 port
                            else port & Port.portId .~ (OutPortId $ Projection (i+1))
                        _                        -> port
                    newPorts    = newPort : newPorts'
                    newPortsMap = Map.fromList $ map (view Port.portId &&& id) newPorts
                localUpdateNode $ node & Node.ports .~ newPortsMap
                moveConnectionsWithPorts portRef
                return True
            _          -> return False

moveConnectionsWithPorts :: AnyPortRef -> Command State ()
moveConnectionsWithPorts (OutPortRef' (OutPortRef nodeId (Projection pos))) = do
    graph <- use Global.graph
    let conns = connectionsContainingNode nodeId graph
    forM_ conns $ \conn -> case conn of
        Connection (OutPortRef srcNodeId (Projection i)) connId ->
            when (srcNodeId == nodeId && i >= pos) $
                localAddConnection $ conn & Connection.src . PortRef.srcPortId .~ Projection (i+1)
        _ -> return ()
moveConnectionsWithPorts _ = $notImplemented
