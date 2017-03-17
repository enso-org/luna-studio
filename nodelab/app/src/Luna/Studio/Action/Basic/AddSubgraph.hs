module Luna.Studio.Action.Basic.AddSubgraph where

import qualified Data.Map.Lazy                          as Map
import           Empire.API.Data.PortRef                (InPortRef, OutPortRef, dstNodeId, srcNodeId)
import           Luna.Studio.Action.Basic.AddConnection (localAddConnection)
import           Luna.Studio.Action.Basic.AddNode       (localAddNode)
import           Luna.Studio.Action.Basic.SelectNode    (selectNodes)
import qualified Luna.Studio.Action.Batch               as Batch
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.UUID                (getUUID)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node           (Node, nodeId)
import           Luna.Studio.State.Global               (State)


addSubgraph :: [Node] -> [(OutPortRef, InPortRef)] -> Command State ()
addSubgraph nodes conns = do
    (newNodes, newConns) <- localAddSubgraph nodes conns
    unless (null newNodes && null newConns) $ Batch.addSubgraph newNodes newConns

localAddSubgraph :: [Node] -> [(OutPortRef, InPortRef)] -> Command State ([Node], [(OutPortRef, InPortRef)])
localAddSubgraph nodes conns = do
    (newIds, newNodes) <- fmap unzip $ forM nodes $ \node -> do
        newId <- getUUID
        return $ (newId, node & nodeId .~ newId)
    let idMapping = Map.fromList $ flip zip newIds $ map (view nodeId) nodes
        newConns  = flip map conns $
            ( \(src, dst) -> maybe (src, dst) (\nid -> (src & srcNodeId .~ nid, dst)) $ Map.lookup (src ^. srcNodeId) idMapping ) .
            ( \(src, dst) -> maybe (src, dst) (\nid -> (src, dst & dstNodeId .~ nid)) $ Map.lookup (dst ^. dstNodeId) idMapping )
    localUpdateSubgraph newNodes newConns
    selectNodes newIds
    return (newNodes, newConns)

localUpdateSubgraph :: [Node] -> [(OutPortRef, InPortRef)] -> Command State ()
localUpdateSubgraph nodes conns = do
    mapM_ localAddNode nodes
    mapM_ (uncurry localAddConnection) conns
