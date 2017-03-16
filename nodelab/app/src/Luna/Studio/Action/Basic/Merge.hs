--TODO[PM, LJK]: make sure that those operations updates everything around and refactor
module Luna.Studio.Action.Basic.Merge where

import           Data.Position                           (fromTuple)
import qualified Data.Position                           as Position
import           Empire.API.Data.Graph                   (Graph)
import qualified Empire.API.Data.Graph                   as GraphAPI
import qualified Empire.API.Data.Node                    as NodeAPI
import           Luna.Studio.Action.Basic.AddConnection  (localConnect)
import           Luna.Studio.Action.Basic.AddNode        (localAddNodes)
import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNode)
import           Luna.Studio.Action.Basic.RemoveNode     (localRemoveNodes)
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.State.NodeEditor     (getNode, modifyNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node            (Node, NodeId, Subgraph)
import qualified Luna.Studio.React.Model.Node            as Node
import           Luna.Studio.State.Global                (State)


localMerge :: NodeId -> [Graph] -> Command State ()
localMerge parentId graphs = withJustM (getNode parentId) $ \parentNode -> do
    subgraphs  <- forM graphs $ \graph -> do
        let (edges, nodes) = partition NodeAPI.isEdge $ graph ^. GraphAPI.nodes
            connections    = graph ^. GraphAPI.connections
            monads         = graph ^. GraphAPI.monads
            nodesPos       = map (fromTuple . view NodeAPI.position) nodes
            topLeft        = fromMaybe def $ Position.leftTopPoint nodesPos
            parentPos      = parentNode ^. Node.position
            movedNodes     = map (NodeAPI.position %~ Position.onTuple (\p -> p - topLeft + parentPos)) nodes
        localAddNodes movedNodes
        mapM_ (uncurry localConnect) connections
        return $ Node.Subgraph (view NodeAPI.nodeId <$> nodes) (convert <$> edges) monads
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
localUnmergeSubgraph = void . localRemoveNodes . view Node.nodes
