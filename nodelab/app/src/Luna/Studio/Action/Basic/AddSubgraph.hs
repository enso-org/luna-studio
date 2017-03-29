module Luna.Studio.Action.Basic.AddSubgraph where

import qualified Data.Map.Lazy                               as Map
import qualified Empire.API.Data.NodeLoc                     as NodeLoc
import           Luna.Studio.Action.Basic.AddConnection      (localAddConnection)
import           Luna.Studio.Action.Basic.AddNode            (localAddExpressionNodes)
import           Luna.Studio.Action.Basic.SelectNode         (selectNodes)
import qualified Luna.Studio.Action.Batch                    as Batch
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.UUID                     (getUUID)
import           Luna.Studio.Data.PortRef                    (InPortRef, OutPortRef, dstNodeLoc, srcNodeLoc)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, nodeLoc)
import           Luna.Studio.State.Global                    (State)


addSubgraph :: [ExpressionNode] -> [(OutPortRef, InPortRef)] -> Command State ()
addSubgraph nodes conns = do
    (newNodes, newConns) <- localAddSubgraph nodes conns
    unless (null newNodes && null newConns) $ Batch.addSubgraph newNodes newConns

localAddSubgraph :: [ExpressionNode] -> [(OutPortRef, InPortRef)] -> Command State ([ExpressionNode], [(OutPortRef, InPortRef)])
localAddSubgraph nodes conns = do
    (newLocs, newNodes) <- fmap unzip $ forM nodes $ \node -> do
        newId <- getUUID
        let newLoc = (node ^. nodeLoc) & NodeLoc.nodeId .~ newId
        return $ (newLoc, node & nodeLoc .~ newLoc)
    let idMapping = Map.fromList $ flip zip newLocs $ map (view nodeLoc) nodes
        newConns  = flip map conns $
            ( \(src, dst) -> maybe (src, dst) (\nl -> (src & srcNodeLoc .~ nl, dst)) $ Map.lookup (src ^. srcNodeLoc) idMapping ) .
            ( \(src, dst) -> maybe (src, dst) (\nl -> (src, dst & dstNodeLoc .~ nl)) $ Map.lookup (dst ^. dstNodeLoc) idMapping )
    localUpdateSubgraph newNodes newConns
    selectNodes newLocs
    return (newNodes, newConns)

localUpdateSubgraph :: [ExpressionNode] -> [(OutPortRef, InPortRef)] -> Command State ()
localUpdateSubgraph nodes conns = do
    localAddExpressionNodes nodes
    mapM_ (uncurry localAddConnection) conns
