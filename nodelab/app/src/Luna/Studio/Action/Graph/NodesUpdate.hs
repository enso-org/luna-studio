module Luna.Studio.Action.Graph.NodesUpdate where

import           Control.Monad.State                (modify)
import qualified Data.Map.Lazy                      as Map
import           Data.Position                      (Position, vector)
import           Data.Text                          (Text)
import           Data.Vector                        (toTuple)
import           Empire.API.Data.Node               (Node (Node), NodeType (ExpressionNode))
import qualified Empire.API.Data.Node               as Node
import qualified Empire.API.Data.NodeMeta           as NodeMeta
import           Empire.API.Data.Port               (InPort (Arg), OutPort (All), Port (Port), PortId (InPortId, OutPortId),
                                                     PortState (NotConnected))
import           Empire.API.Data.TypeRep            (TypeRep (TStar))
import qualified Empire.API.Graph.AddNode           as AddNode
import qualified Luna.Studio.Action.Batch           as Batch
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Connect         ()
import           Luna.Studio.Action.ConnectionPen   ()
import           Luna.Studio.Action.Graph.Focus     (focusNode)
import           Luna.Studio.Action.Graph.Selection (selectNodes, selectedNodes)
import           Luna.Studio.Action.Graph.Update    (updateConnectionsForNodes)
import           Luna.Studio.Action.Node.Snap       (snap)
import           Luna.Studio.Action.Port.Self       (showOrHideSelfPort)
import           Luna.Studio.Action.UUID            (getUUID)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Action           (connectAction, penConnectAction)
import           Luna.Studio.State.Global           (State, checkAction)
import qualified Luna.Studio.State.Global           as Global
import qualified Luna.Studio.State.Graph            as Graph


localUpdateNodes :: [Node] -> Command State ()
localUpdateNodes = mapM_ localUpdateNode

localUpdateNode :: Node -> Command State ()
localUpdateNode node = do
    zoom Global.graph $ modify $ Graph.addNode node
    mayConnect    <- checkAction connectAction
    mayPenConnect <- checkAction penConnectAction
    nodeModel'    <- showOrHideSelfPort mayConnect mayPenConnect $ Model.fromNode node
    let nodeId = node ^. Node.nodeId
    Global.modifyNodeEditor $ do
        maySelection <- (fmap . fmap) (view Model.isSelected) $ use $ NodeEditor.nodes . at nodeId
        let nodeModel = case maySelection of
                Nothing        -> nodeModel'
                Just selection -> nodeModel' & Model.isSelected .~ selection
        NodeEditor.nodes . at nodeId ?= nodeModel
    updateConnectionsForNodes [nodeId]
