module Luna.Studio.Action.Graph.AddPort where

import           Control.Arrow
import qualified Data.Map.Lazy                    as Map
import           Empire.API.Data.Node             (NodeId, NodeType (InputEdge, OutputEdge))
import qualified Empire.API.Data.Node             as Node
import           Empire.API.Data.Port             (OutPort (Projection), Port (Port), PortId (OutPortId), PortState (NotConnected))
import qualified Empire.API.Data.Port             as Port
import           Empire.API.Data.TypeRep          (TypeRep (TStar))
import qualified Luna.Studio.Action.Batch         as Batch
import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Action.Graph.AddNode (localUpdateNode)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global         (State)
import qualified Luna.Studio.State.Global         as Global
import qualified Luna.Studio.State.Graph          as Graph

addPort :: NodeId -> Int -> Command State ()
addPort nodeId pos = localAddPort nodeId pos >> Batch.addPort nodeId pos

localAddPort :: NodeId -> Int -> Command State ()
localAddPort nodeId pos = do
    mayNode <- use $ Global.graph . Graph.nodesMap . at nodeId
    withJust mayNode $ \node -> case node ^. Node.nodeType of
        OutputEdge -> $notImplemented
        InputEdge  -> do
            let newPort     = Port (OutPortId $ Projection pos) "" TStar NotConnected
                oldPorts    = node ^. Node.ports . to Map.elems
                newPorts'   = flip map oldPorts $ \port -> case port ^. Port.portId of
                    OutPortId (Projection i) -> if i < pos then
                             port
                        else port & Port.portId .~ (OutPortId $ Projection (i+1))
                    _                        -> port
                newPorts    = newPort : newPorts'
                newPortsMap = Map.fromList $ map (view Port.portId &&& id) newPorts
            localUpdateNode $ node & Node.ports .~ newPortsMap
        _          -> return ()
