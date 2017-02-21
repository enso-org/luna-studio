module Luna.Studio.Action.Graph.Create
    ( createGraph
    ) where

import qualified Data.HashMap.Lazy                as HashMap
import           Empire.API.Data.Node             (Node, NodeId)
import qualified Empire.API.Data.Node             as Node
import           Empire.API.Data.PortRef          (InPortRef, OutPortRef)
import           Empire.API.Data.TypeRep          (TypeRep)
import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Action.Graph.Connect (localConnectNodes)
import           Luna.Studio.Action.Graph.Focus   (updateNodeZOrder)
import           Luna.Studio.Action.Graph.Update  (updateMonads)
import           Luna.Studio.Action.Node.Create   (registerNode)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global         (State)
import qualified Luna.Studio.State.Global         as Global
import qualified Luna.Studio.State.Graph          as Graph



fastAddNodes :: [Node] -> Command State ()
fastAddNodes nodes = do
    let nodeIds = view Node.nodeId <$> nodes
    Global.graph . Graph.nodesMap .= HashMap.fromList (nodeIds `zip` nodes)
    mapM_ registerNode nodes

createGraph :: [Node] -> [(OutPortRef, InPortRef)] -> [(TypeRep, [NodeId])] -> Command State ()
createGraph nodes connections monads = do
    fastAddNodes nodes
    mapM_ (uncurry localConnectNodes) connections
    updateMonads monads
    updateNodeZOrder
