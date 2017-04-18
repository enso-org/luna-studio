module Luna.Studio.Action.Basic.AddNode where

import           Data.Position                           (Position, toTuple)
import           Data.Text                               (Text)
import           Data.Vector2                             ()
import           Empire.API.Data.LabeledTree             (LabeledTree (LabeledTree))
import qualified Empire.API.Data.Node                    as Empire
import           Empire.API.Data.NodeMeta                (NodeMeta (NodeMeta))
import           Empire.API.Data.Port                    (InPortIndex (Arg, Self), Port (Port), PortState (NotConnected))
import           Empire.API.Data.TypeRep                 (TypeRep (TStar))
import qualified JS.GoogleAnalytics                      as GA
import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNode)
import           Luna.Studio.Action.Basic.FocusNode      (focusNode)
import           Luna.Studio.Action.Basic.SelectNode     (selectNode)
import qualified Luna.Studio.Action.Batch                as Batch
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.Node.Snap            (snap)
import           Luna.Studio.Action.State.Model          (shouldDisplayPortSelf)
import           Luna.Studio.Action.State.NodeEditor     (getSelectedNodes)
import qualified Luna.Studio.Action.State.NodeEditor     as NodeEditor
import           Luna.Studio.Action.UUID                 (getUUID)
import           Luna.Prelude
import           Luna.Studio.React.Model.Node            (ExpressionNode, NodeLoc (NodeLoc), NodePath, inPortAt, nodeLoc)
import           Luna.Studio.React.Model.Port            (Mode (Invisible), ensureVisibility, mode)
import           Luna.Studio.State.Global                (State)


createNode :: NodePath -> Position -> Text -> Command State ()
createNode parentPath nodePos expr = do
    selected <- getSelectedNodes
    nid      <- getUUID
    let snappedPos  = snap nodePos
        nodeMeta    = NodeMeta (toTuple snappedPos) True
        connectTo   = if length selected == 1
                      then view nodeLoc <$> listToMaybe selected
                      else Nothing
        defInPorts  = LabeledTree def $ Port [Arg 0] "" TStar NotConnected
        defOutPorts = LabeledTree def $ Port [] "" TStar NotConnected
        empireNode  = Empire.ExpressionNode nid expr def def defInPorts defOutPorts nodeMeta False
        node        = convert (parentPath, empireNode)
        nl          = NodeLoc parentPath nid
    localAddExpressionNode node
    selectNode nl
    Batch.addNode nl expr snappedPos True connectTo
    GA.sendEvent $ GA.AddNode $ if isJust connectTo then GA.AutoConnect else GA.Simple

localAddExpressionNodes :: [ExpressionNode] -> Command State ()
localAddExpressionNodes = mapM_ localAddExpressionNode

localAddExpressionNode :: ExpressionNode -> Command State ()
localAddExpressionNode node = do
    selfPortVis <- shouldDisplayPortSelf $ node
    let selfMode = if selfPortVis then ensureVisibility else const Invisible
        node' = node & inPortAt [Self] . mode %~ selfMode
    NodeEditor.addExpressionNode node'
    void . redrawConnectionsForNode $ node ^. nodeLoc
    focusNode $ node ^. nodeLoc
