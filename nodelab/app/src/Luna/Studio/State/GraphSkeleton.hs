module Luna.Studio.State.GraphSkeleton where

import           Luna.Studio.Prelude

import           Data.Aeson
import qualified Data.Set                   as Set

import           Empire.API.Data.Connection (Connection)
import           Empire.API.Data.Node       (Node, NodeId)
import qualified Empire.API.Data.Node       as Node
import           Luna.Studio.State.Graph       (State, connectionsToNodes, getNodes)


data GraphSkeleton = GraphSkeleton { _nodesList       :: [Node]
                                   , _connectionsList :: [Connection]
                                   } deriving (Show, Eq, Generic)

makeLenses ''GraphSkeleton
instance ToJSON GraphSkeleton
instance FromJSON GraphSkeleton

separateSubgraph :: [NodeId] -> State -> GraphSkeleton
separateSubgraph nodeIds' state = do
  let nodeIds = Set.fromList nodeIds'
      nodes   = filter (flip Set.member nodeIds . view Node.nodeId) $ getNodes state
  GraphSkeleton nodes (connectionsToNodes nodeIds state)
