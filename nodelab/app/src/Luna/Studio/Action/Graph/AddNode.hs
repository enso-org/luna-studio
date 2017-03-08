module Luna.Studio.Action.Graph.AddNode where

import           Control.Monad.State                (modify)
import           Data.Position                      (Position, vector)
import           Data.Text                          (Text)
import           Data.Vector                        (toTuple)
import           Empire.API.Data.Node               (Node (Node), NodeType (ExpressionNode))
import qualified Empire.API.Data.Node               as Node
import qualified Empire.API.Data.NodeMeta           as NodeMeta
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


addNode :: Position -> Text -> Command State ()
addNode nodePos expression = do
    selected <- selectedNodes
    nodeId   <- getUUID
    let nodeType = ExpressionNode expression
        nodeMeta = def & NodeMeta.position .~ toTuple ((snap nodePos) ^. vector)
        connectTo = if length selected == 1 then
                 Just $ (head selected) ^. Model.nodeId
            else Nothing
        node     = Node nodeId def nodeType False def nodeMeta def
    localAddNode node
    selectNodes [node ^. Node.nodeId]
    Batch.addNode nodeId expression nodeMeta connectTo
    -- TODO: Is this even used???
    -- GA.sendEvent $ GA.AddNode $ if isJust connectTo then GA.AutoConnect else GA.Simple

localAddNode :: Node -> Command State ()
localAddNode node = localUpdateNode node >> focusNode (node ^. Node.nodeId)

localUpdateNode :: Node -> Command State ()
localUpdateNode node = do
    zoom Global.graph $ modify $ Graph.addNode node
    mayConnect    <- checkAction connectAction
    mayPenConnect <- checkAction penConnectAction
    nodeModel     <- showOrHideSelfPort mayConnect mayPenConnect $ Model.fromNode node
    let nodeId = node ^. Node.nodeId
    Global.modifyNodeEditor $ NodeEditor.nodes . at nodeId ?= nodeModel
    updateConnectionsForNodes [nodeId]
