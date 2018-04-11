module NodeEditor.Action.Basic.AddSubgraph where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified Data.Map.Lazy                              as Map
import           LunaStudio.Data.Connection                 (Connection (Connection))
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import           LunaStudio.Data.PortRef                    (dstNodeLoc, srcNodeLoc)
import           NodeEditor.Action.Basic.AddConnection      (localAddConnection)
import           NodeEditor.Action.Basic.AddNode            (localAddExpressionNodes)
import           NodeEditor.Action.Basic.SelectNode         (selectNodes)
import qualified NodeEditor.Action.Batch                    as Batch
import           NodeEditor.Action.UUID                     (getUUID)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, nodeLoc)
import           NodeEditor.State.Global                    (State)


addSubgraph :: [ExpressionNode] -> [Connection] -> Command State ()
addSubgraph nodes conns = do
    (newNodes, newConns) <- localAddSubgraph nodes conns
    unless (null newNodes && null newConns)
        $ Batch.addSubgraph newNodes newConns

localAddSubgraph :: [ExpressionNode] -> [Connection]
    -> Command State ([ExpressionNode], [Connection])
localAddSubgraph nodes conns = do
    (newLocs, newNodes) <- fmap unzip $ forM nodes $ \node -> do
        newId <- getUUID
        let newLoc = (node ^. nodeLoc) & NodeLoc.nodeId .~ newId
        return (newLoc, node & nodeLoc .~ newLoc)
    let idMapping = Map.fromList $ flip zip newLocs $ map (view nodeLoc) nodes
        setSrcNl (Connection src dst) = maybe
            (Connection src dst)
            (\nl -> Connection (src & srcNodeLoc .~ nl) dst)
            $ Map.lookup (src ^. srcNodeLoc) idMapping
        setDstNl (Connection src dst) = maybe
            (Connection src dst)
            (\nl -> Connection src (dst & dstNodeLoc .~ nl))
            $ Map.lookup (dst ^. dstNodeLoc) idMapping
        newConns  = (setSrcNl . setDstNl) <$> conns
    localUpdateSubgraph newNodes newConns
    selectNodes newLocs
    return (newNodes, newConns)

localUpdateSubgraph :: [ExpressionNode] -> [Connection] -> Command State ()
localUpdateSubgraph nodes conns = do
    localAddExpressionNodes nodes
    mapM_ localAddConnection conns
