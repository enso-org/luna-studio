module Luna.Studio.Action.Graph.Subgraph
    ( localMerge
    , localUnmerge
    ) where

import           Data.Position                    (Position(Position))
import qualified Data.Position                    as Position

import           Empire.API.Data.Graph            (Graph)
import qualified Empire.API.Data.Graph            as GraphAPI
import qualified Empire.API.Data.Node             as NodeAPI
import           Luna.Studio.Action.Command       (Command)
import qualified Luna.Studio.Action.Graph.Connect as Connect
import qualified Luna.Studio.Action.Graph.Create  as Create
import qualified Luna.Studio.Action.Node.Remove   as Remove
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node     (Node, NodeId, Subgraph)
import qualified Luna.Studio.React.Model.Node     as Node
import           Luna.Studio.State.Global         (State)
import qualified Luna.Studio.State.Global         as Global


localMerge :: NodeId -> [Graph] -> Command State ()
localMerge parentId graphs = withJustM (Global.getNode parentId) $ \parentNode -> do
    subgraphs  <- forM graphs $ \graph -> do
        let (edges, nodes) = partition NodeAPI.isEdge $ graph ^. GraphAPI.nodes
            connections    = graph ^. GraphAPI.connections
            monads         = graph ^. GraphAPI.monads
            nodesPos       = map (Position . Position.fromTuple . view NodeAPI.position) nodes
            topLeft        = fromMaybe def $ Position.leftTopPoint nodesPos
            parentPos      = parentNode ^. Node.position
            movedNodes     = map (NodeAPI.position %~ Position.onTuple (\p -> p - topLeft + parentPos)) nodes
        Create.addNodes movedNodes
        mapM_ (uncurry Connect.localConnect) connections
        return $ Node.Subgraph (view NodeAPI.nodeId <$> nodes) (Node.fromNode <$> edges) monads
    Global.modifyNode parentId $
        Node.mode .= Node.Expanded (Node.Function subgraphs)

localUnmerge :: Node -> Command State ()
localUnmerge node = case node ^. Node.mode of
    Node.Expanded (Node.Function subgraphs) -> do
        mapM_ localUnmergeSubgraph subgraphs
        Global.modifyNode (node ^. Node.nodeId) $
            Node.mode .= Node.Collapsed
    _ -> return ()

localUnmergeSubgraph :: Subgraph -> Command State ()
localUnmergeSubgraph subgraph = do
    Remove.localRemoveNodes $ subgraph ^. Node.nodes
