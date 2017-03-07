module Luna.Studio.Action.Graph.AddSubgraph where

import qualified Data.Map.Lazy                    as Map
import           Empire.API.Data.Connection       (Connection)
import qualified Empire.API.Data.Connection       as Connection
import           Empire.API.Data.Node             (Node)
import qualified Empire.API.Data.Node             as Node
import qualified Empire.API.Data.PortRef          as PortRef
import qualified Luna.Studio.Action.Batch         as Batch
import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Action.Graph.AddNode (localAddNode)
import           Luna.Studio.Action.Graph.Connect (localAddConnection)
import           Luna.Studio.Action.UUID          (getUUID)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global         (State)


addSubgraph :: [Node] -> [Connection] -> Command State ()
addSubgraph nodes conns = localAddSubgraph nodes conns >>= (uncurry Batch.addSubgraph)

localAddSubgraph :: [Node] -> [Connection] -> Command State ([Node], [Connection])
localAddSubgraph nodes conns = do
    (oldIdNewNodes) <- forM nodes $ \node -> do
        newNodeId <- getUUID
        return $ (node ^. Node.nodeId, node & Node.nodeId .~ newNodeId)
    let idMapping = Map.fromList $ flip map oldIdNewNodes $ \(oldId, node) -> (oldId, node ^. Node.nodeId)
        newNodes  = map snd oldIdNewNodes
        newConns  = flip map conns $ \conn -> do
            let srcNodeId = conn ^. Connection.src . PortRef.srcNodeId
                dstNodeId = conn ^. Connection.dst . PortRef.dstNodeId
                conn'     = maybe conn
                                ( \nodeId -> conn & Connection.src . PortRef.srcNodeId .~ nodeId )
                                $ Map.lookup srcNodeId idMapping
            maybe conn'
                ( \nodeId -> conn' & Connection.dst . PortRef.dstNodeId .~ nodeId )
                $ Map.lookup dstNodeId idMapping
    mapM_ localAddNode newNodes
    mapM_ localAddConnection newConns
    return (newNodes, newConns)
