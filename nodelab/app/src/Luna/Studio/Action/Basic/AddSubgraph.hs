module Luna.Studio.Action.Basic.AddSubgraph where

import qualified Data.Map.Lazy                          as Map
import           Empire.API.Data.Connection             (Connection, dst, dstNodeId, src, srcNodeId)
import qualified Empire.API.Data.PortRef                as PortRef
import           Luna.Studio.Action.Basic.AddConnection (localAddConnection)
import           Luna.Studio.Action.Basic.AddNode       (localAddNode)
import           Luna.Studio.Action.Basic.SelectNode    (selectNodes)
import qualified Luna.Studio.Action.Batch               as Batch
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.UUID                (getUUID)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node           (Node, nodeId)
import           Luna.Studio.State.Global               (State)


addSubgraph :: [Node] -> [Connection] -> Command State ()
addSubgraph nodes conns = do
    (newNodes, newConns) <- localAddSubgraph nodes conns
    unless (null newNodes && null newConns) $ Batch.addSubgraph (convert <$> nodes) conns

localAddSubgraph :: [Node] -> [Connection] -> Command State ([Node], [Connection])
localAddSubgraph nodes conns = do
    (newIds, newNodes) <- fmap unzip $ forM nodes $ \node -> do
        newId <- getUUID
        return $ (newId, node & nodeId .~ newId)
    let idMapping = Map.fromList $ flip zip newIds $ map (view nodeId) nodes
        newConns  = flip map conns $
            ( \conn -> maybe conn (\nid -> conn & src . PortRef.srcNodeId .~ nid ) $ Map.lookup (conn ^. srcNodeId) idMapping ) .
            ( \conn -> maybe conn (\nid -> conn & dst . PortRef.dstNodeId .~ nid ) $ Map.lookup (conn ^. dstNodeId) idMapping )
    localUpdateSubgraph newNodes newConns
    selectNodes newIds
    return (newNodes, newConns)

localUpdateSubgraph :: [Node] -> [Connection] -> Command State ()
localUpdateSubgraph nodes conns = do
    mapM_ localAddNode nodes
    mapM_ localAddConnection conns
