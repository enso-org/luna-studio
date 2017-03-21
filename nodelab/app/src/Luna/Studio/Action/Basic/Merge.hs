--TODO[PM, LJK]: make sure that those operations updates everything around and refactor
module Luna.Studio.Action.Basic.Merge where

import qualified Data.HashMap.Strict                       as HashMap
import           Empire.API.Data.Graph                     (Graph)
import qualified Empire.API.Data.Graph                     as GraphAPI
import           Luna.Studio.Action.Basic.AddConnection    (localAddConnections)
import           Luna.Studio.Action.Basic.DrawConnection   (redrawConnectionsForNode)
import           Luna.Studio.Action.Basic.RemoveConnection (localRemoveConnectionsContainingNodes)
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Action.State.NodeEditor       (modifyNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node              (Node, NodeId, Subgraph)
import qualified Luna.Studio.React.Model.Node              as Node
import           Luna.Studio.State.Global                  (State)


localMerge :: NodeId -> [Graph] -> Command State ()
localMerge parentId graphs = do
    subgraphs <- forM graphs $ \graph -> do
        let nodes       = convert <$> graph ^. GraphAPI.nodes
            connections = graph ^. GraphAPI.connections
            monads      = graph ^. GraphAPI.monads
        void $ localAddConnections connections
        return $ Node.Subgraph (Node.toNodesMap nodes) monads
    modifyNode parentId $ Node.mode .= Node.Expanded (Node.Function subgraphs)
    void $ redrawConnectionsForNode parentId

localUnmerge :: Node -> Command State ()
localUnmerge node = case node ^. Node.mode of
    Node.Expanded (Node.Function subgraphs) -> do
        mapM_ localUnmergeSubgraph subgraphs
        modifyNode (node ^. Node.nodeId) $
            Node.mode .= Node.Collapsed
    _ -> return ()

localUnmergeSubgraph :: Subgraph -> Command State ()
localUnmergeSubgraph = void . localRemoveConnectionsContainingNodes . HashMap.keys . view Node.nodes
