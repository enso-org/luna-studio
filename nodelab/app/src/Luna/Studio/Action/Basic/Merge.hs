module Luna.Studio.Action.Basic.Merge where

import qualified Data.HashMap.Strict                         as HashMap
import           Data.Map.Lazy                               (Map)
import qualified Data.Map.Lazy                               as Map
import           Empire.API.Data.Breadcrumb                  (BreadcrumbItem)
import           Empire.API.Data.Graph                       (Graph)
import qualified Empire.API.Data.Graph                       as GraphAPI
import qualified Empire.API.Data.NodeLoc                     as NodeLoc
import           Luna.Studio.Action.Basic.AddConnection      (localAddConnections)
import           Luna.Studio.Action.Basic.DrawConnection     (redrawConnectionsForNode)
import           Luna.Studio.Action.Basic.RemoveConnection   (localRemoveConnectionsContainingNodes)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node                (ExpressionNode, NodePath, nodeLoc, toEdgeNodesMap, toExpressionNodesMap,
                                                              _Edge, _Expression)
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpandedMode (Function), Mode (Collapsed, Expanded), Subgraph (Subgraph),
                                                              expressionNodes, mode)
import           Luna.Studio.State.Global                    (State)


localMerge :: NodePath -> Map BreadcrumbItem Graph -> Command State ()
localMerge parentPath graphs = do
    subgraphs <- forM (Map.toList graphs) $ \(k, graph) -> do
        let allNodes           = (convert . (NodeLoc.appendItem k parentPath,) <$> graph ^. GraphAPI.nodes)
            expressionNodesMap = toExpressionNodesMap $ allNodes ^.. traverse . _Expression
            edgeNodesMap       = toEdgeNodesMap $ allNodes ^.. traverse . _Edge
            connections        = map ((_1 %~ convert . (parentPath,)) . (_2 %~ convert . (parentPath,))) $ graph ^. GraphAPI.connections
            monads             = graph ^. GraphAPI.monads
        void $ localAddConnections connections
        return (k, Subgraph expressionNodesMap edgeNodesMap monads)
    let parentLoc = NodeLoc.fromPath parentPath
    modifyExpressionNode parentLoc $ mode .= Expanded (Function $ Map.fromList subgraphs)
    void $ redrawConnectionsForNode parentLoc

localUnmerge :: ExpressionNode -> Command State ()
localUnmerge node = case node ^. mode of
    Expanded (Function subgraphs) -> do
        mapM_ localUnmergeSubgraph subgraphs
        modifyExpressionNode (node ^. nodeLoc) $
            mode .= Collapsed
    _ -> return ()


--TODO[PM]: Review this function - do we need to disconnect edge nodes as well?
localUnmergeSubgraph :: Subgraph -> Command State ()
localUnmergeSubgraph = void . localRemoveConnectionsContainingNodes . map (view nodeLoc) . HashMap.elems . view expressionNodes
