module Node.Editor.Action.Basic.AddSubgraph where

import qualified Data.Map.Lazy                               as Map
import qualified Empire.API.Data.NodeLoc                     as NodeLoc
import           Empire.API.Data.PortRef                     (InPortRef, OutPortRef, dstNodeLoc, srcNodeLoc)
import           Node.Editor.Action.Basic.AddConnection      (localAddConnection)
import           Node.Editor.Action.Basic.AddNode            (localAddExpressionNodes)
import           Node.Editor.Action.Basic.SelectNode         (selectNodes)
import qualified Node.Editor.Action.Batch                    as Batch
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.UUID                     (getUUID)
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (ExpressionNode, nodeLoc)
import           Node.Editor.State.Global                    (State)


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
