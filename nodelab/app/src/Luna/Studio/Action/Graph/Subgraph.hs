module Luna.Studio.Action.Graph.Subgraph
    ( localMerge
    , localUnmerge
    ) where

import qualified Data.Set                         as Set

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
localMerge parentId graphs = do
    subgraphs <- forM graphs $ \graph -> do
        let nodes       = graph ^. GraphAPI.nodes
            connections = graph ^. GraphAPI.connections
            monads      = graph ^. GraphAPI.monads
        Create.addNodes nodes
        mapM_ (uncurry Connect.localConnect) connections
        return $ Node.Subgraph (Set.fromList $ map (view NodeAPI.nodeId) nodes) monads
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
    Remove.localRemoveNodes $ subgraph ^. Node.nodes . to Set.toList
