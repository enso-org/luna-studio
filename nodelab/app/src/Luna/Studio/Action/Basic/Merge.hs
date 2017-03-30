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
import           Luna.Studio.React.Model.Node                (ExpressionNode, NodePath, nodeLoc, toExpressionNodesMap, toSidebarNodesMap,
                                                              _Expression, _Sidebar)
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpandedMode (Function), Mode (Collapsed, Expanded), Subgraph (Subgraph),
                                                              expressionNodes, mode, sidebarNodes)
import           Luna.Studio.State.Global                    (State)


localMerge :: NodePath -> Map BreadcrumbItem Graph -> Command State ()
localMerge parentPath graphs = do
    subgraphs <- forM (Map.toList graphs) $ \(k, graph) -> do
        let allNodes           = (convert . (NodeLoc.replaceLast k parentPath,) <$> graph ^. GraphAPI.nodes)
            expressionNodesMap = toExpressionNodesMap $ allNodes ^.. traverse . _Expression
            sidebarNodesMap    = toSidebarNodesMap    $ allNodes ^.. traverse . _Sidebar
            monads             = graph ^. GraphAPI.monads
        return (k, Subgraph expressionNodesMap sidebarNodesMap monads)
    let parentLoc = NodeLoc.fromPath parentPath
    modifyExpressionNode parentLoc $ mode .= Expanded (Function $ Map.fromList subgraphs)
    forM_ (Map.elems graphs) $ \graph -> do
        let connections        = map ((_1 %~ NodeLoc.prependPath parentPath) . (_2 %~ NodeLoc.prependPath parentPath)) $ graph ^. GraphAPI.connections
        void $ localAddConnections connections
    void $ redrawConnectionsForNode parentLoc

localUnmerge :: ExpressionNode -> Command State ()
localUnmerge node = case node ^. mode of
    Expanded (Function subgraphs) -> do
        mapM_ localUnmergeSubgraph subgraphs
        modifyExpressionNode (node ^. nodeLoc) $
            mode .= Collapsed
    _ -> return ()


--TODO[PM]: Review this function - do we need to disconnect sidebar nodes as well?
localUnmergeSubgraph :: Subgraph -> Command State ()
localUnmergeSubgraph subgraph = do
    let expressionLocs = view nodeLoc <$> (subgraph ^. expressionNodes . to HashMap.elems)
        sidebarLocs    = view nodeLoc <$> (subgraph ^. sidebarNodes . to HashMap.elems)
    void $ localRemoveConnectionsContainingNodes $ expressionLocs <> sidebarLocs
