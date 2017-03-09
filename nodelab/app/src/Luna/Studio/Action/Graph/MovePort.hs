module Luna.Studio.Action.Graph.MovePort where

import           Control.Arrow
import qualified Data.Map.Lazy                        as Map
import           Empire.API.Data.Connection           (Connection (Connection))
import qualified Empire.API.Data.Connection           as Connection
import           Empire.API.Data.Node                 (Node, NodeId, NodeType (InputEdge, OutputEdge))
import qualified Empire.API.Data.Node                 as Node
import           Empire.API.Data.Port                 (OutPort (All, Projection), Port (Port), PortId (InPortId, OutPortId),
                                                       PortState (NotConnected))
import qualified Empire.API.Data.Port                 as Port
import           Empire.API.Data.PortRef              (AnyPortRef (OutPortRef'), OutPortRef (OutPortRef))
import qualified Empire.API.Data.PortRef              as PortRef
import           Empire.API.Data.TypeRep              (TypeRep (TStar))
import qualified Luna.Studio.Action.Batch             as Batch
import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Action.Geometry.Port     (countSameTypePorts)
import           Luna.Studio.Action.Graph.Connect     (localAddConnection)
import           Luna.Studio.Action.Graph.NodesUpdate (localUpdateNode)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global             (State)
import qualified Luna.Studio.State.Global             as Global
import           Luna.Studio.State.Graph              (connectionsContainingNode)
import qualified Luna.Studio.State.Graph              as Graph


addPort :: AnyPortRef -> AnyPortRef -> Command State ()
addPort srcPortRef dstPortRef = whenM (localMovePort srcPortRef dstPortRef) $
    Batch.movePort srcPortRef dstPortRef

--TODO[LJK, PM, JK]: If this action returns False we should notify user about it since
-- GUI will looks like everything was send to backend but nothing happens
localMovePort :: AnyPortRef -> AnyPortRef -> Command State Bool
localMovePort srcPortRef dstPortRef = do
    if srcPortRef ^. PortRef.nodeId /= dstPortRef ^. PortRef.nodeId then
        return False
    else do
        let nodeId    = srcPortRef ^. PortRef.nodeId
            srcPortId = srcPortRef ^. PortRef.portId
            dstPortId = dstPortRef ^. PortRef.portId
        mayNode <- use $ Global.graph . Graph.nodesMap . at nodeId
        case join $ nodeToUpdate srcPortRef dstPortRef <$> mayNode of
            Nothing   -> return False
            Just node -> do
                localUpdateNode node
                moveConnectionsWithPorts srcPortRef dstPortRef
                return True

countProjectionPorts :: [Port] -> Int
countProjectionPorts ports = length $ flip filter ports $ \port ->
    case port ^. Port.portId of
        OutPortId (Projection _) -> True
        _                        -> False

nodeToUpdate :: AnyPortRef -> AnyPortRef -> Node -> Maybe Node
nodeToUpdate srcPortRef@(OutPortRef' (OutPortRef srcNodeId (Projection src))) dstPortRef@(OutPortRef' (OutPortRef dstNodeId (Projection dst))) node = do
    let portsMap = node ^. Node.ports
        ports    = Map.elems portsMap
    if not ( node ^. Node.nodeType == Node.InputEdge
          && srcNodeId == dstNodeId
          && Map.member (srcPortRef ^. PortRef.portId) portsMap
          && dst < countProjectionPorts ports ) then do
              Nothing
    else do
        let newPorts = flip map ports $ \port -> case port ^. Port.portId of
                OutPortId (Projection i) ->
                    if      i == src            then port & Port.portId .~ (OutPortId $ Projection dst)
                    else if i > src && i <= dst then port & Port.portId .~ (OutPortId $ Projection (i-1))
                    else if i < src && i >= dst then port & Port.portId .~ (OutPortId $ Projection (i+1))
                    else port
                _                        -> port
            newPortsMap = Map.fromList $ map (view Port.portId &&& id) newPorts
        Just $ node & Node.ports .~ newPortsMap
nodeToUpdate _ _ _ = $notImplemented


moveConnectionsWithPorts :: AnyPortRef -> AnyPortRef -> Command State ()
moveConnectionsWithPorts (OutPortRef' (OutPortRef srcNodeId (Projection src))) (OutPortRef' (OutPortRef dstNodeId (Projection dst))) =
    when (srcNodeId == dstNodeId) $ do
        graph <- use Global.graph
        let conns = connectionsContainingNode srcNodeId graph
        forM_ conns $ \conn -> case conn of
            Connection (OutPortRef nodeId (Projection i)) connId -> when (srcNodeId == nodeId) $
                if      i == src            then void $ localAddConnection $ conn & Connection.src . PortRef.srcPortId .~ Projection dst
                else if i > src && i <= dst then void $ localAddConnection $ conn & Connection.src . PortRef.srcPortId .~ Projection (i-1)
                else if i < src && i >= dst then void $ localAddConnection $ conn & Connection.src . PortRef.srcPortId .~ Projection (i+1)
                else                             return ()
            _ -> return ()
moveConnectionsWithPorts _ _ = $notImplemented
