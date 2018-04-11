module NodeEditor.Action.Basic.Merge where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified Data.HashMap.Strict                        as HashMap
import           Data.Map.Lazy                              (Map)
import qualified Data.Map.Lazy                              as Map
import           LunaStudio.Data.Breadcrumb                 (BreadcrumbItem)
import qualified LunaStudio.Data.Connection                 as Connection
import           LunaStudio.Data.Graph                      (Graph)
import qualified LunaStudio.Data.Graph                      as GraphAPI
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import           NodeEditor.Action.Basic.AddConnection      (localAddConnections)
import           NodeEditor.Action.Basic.RemoveConnection   (localRemoveConnectionsContainingNodes)
import           NodeEditor.Action.State.NodeEditor         (modifyExpressionNode)
import           NodeEditor.React.Model.Node                (ExpressionNode, NodePath, nodeLoc, toNodesMap)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpandedMode (Function), Mode (Collapsed, Expanded), Subgraph (Subgraph),
                                                             expressionNodes, inputNode, mode, outputNode)
import           NodeEditor.State.Global                    (State)


localMerge :: NodePath -> Map BreadcrumbItem Graph -> Command State ()
localMerge parentPath graphs = do
    subgraphs <- forM (Map.toList graphs) $ \(k, graph) -> do
        let path =  NodeLoc.replaceLast k parentPath
            expressionNodesMap = toNodesMap
                (convert . (path, ) <$> (graph ^. GraphAPI.nodes))
            mayInputNode  = convert . (path, ) <$> graph ^. GraphAPI.inputSidebar
            mayOutputNode = convert . (path, ) <$> graph ^. GraphAPI.outputSidebar
            monads        = graph ^. GraphAPI.monads
        return (k, Subgraph expressionNodesMap mayInputNode mayOutputNode monads)
    let parentLoc = NodeLoc.fromPath parentPath
    modifyExpressionNode parentLoc $
        mode .= Expanded (Function $ Map.fromList subgraphs)
    forM_ (Map.elems graphs) $ \graph -> do
        let connections = Connection.prependPath parentPath
                <$> graph ^. GraphAPI.connections
        void $ localAddConnections connections

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
    let expressionLocs = view nodeLoc
            <$> (subgraph ^. expressionNodes . to HashMap.elems)
        inputLoc  = view nodeLoc <$> (subgraph ^. inputNode )
        outputLoc = view nodeLoc <$> (subgraph ^. outputNode)
    void $ localRemoveConnectionsContainingNodes
        $ expressionLocs <> maybeToList inputLoc <> maybeToList outputLoc
