module Luna.Studio.Action.Graph.NodeResultUpdate where

import           Control.Arrow                      ((&&&))
import           Control.Monad.State                (modify)
import qualified Data.Map.Lazy                      as Map
import           Empire.API.Data.Node               (Node, NodeId, NodeTypecheckerUpdate)
import qualified Empire.API.Data.Node               as Node
import           Empire.API.Graph.NodeResultUpdate  (NodeValue)
import qualified Luna.Studio.Action.Batch           as BatchCmd
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         ()
import           Luna.Studio.Action.ConnectionPen   ()
import           Luna.Studio.Action.Graph.Update    (updateConnectionsForNodes)
-- import           Luna.Studio.Action.Node.Create     (addNode)
import           Luna.Studio.Action.Port.Self       (showOrHideSelfPort)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node       (makePorts)
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.React.Model.Port       (portId)
import           Luna.Studio.State.Action           (connectAction, penConnectAction)
import           Luna.Studio.State.Global           (State, checkAction)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph


updateNodeValue :: NodeId -> NodeValue -> Command State ()
updateNodeValue nodeId val =
    Global.modifyNode nodeId $ Model.value ?= val

updateNodeProfilingData :: NodeId -> Integer -> Command State ()
updateNodeProfilingData nodeId execTime =
    Global.modifyNode nodeId $ Model.execTime ?= execTime
