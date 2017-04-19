module Node.Editor.Action.Basic.AddNode where

import           Data.Position                       (Position, toTuple)
import           Data.Text                           (Text)
import           Empire.API.Data.LabeledTree         (LabeledTree (LabeledTree))
import qualified Empire.API.Data.Node                as Empire
import           Empire.API.Data.NodeMeta            (NodeMeta (NodeMeta))
import           Empire.API.Data.Port                (InPortIndex (Arg, Self), Port (Port), PortState (NotConnected))
import           Empire.API.Data.TypeRep             (TypeRep (TStar))
import qualified JS.GoogleAnalytics                  as GA
import           Node.Editor.Action.Basic.FocusNode  (focusNode)
import           Node.Editor.Action.Basic.SelectNode (selectNode)
import qualified Node.Editor.Action.Batch            as Batch
import           Node.Editor.Action.Command          (Command)
import           Node.Editor.Action.Node.Snap        (snap)
import           Node.Editor.Action.State.Model      (shouldDisplayPortSelf)
import           Node.Editor.Action.State.NodeEditor (getSelectedNodes)
import qualified Node.Editor.Action.State.NodeEditor as NodeEditor
import           Node.Editor.Action.UUID             (getUUID)
import           Luna.Prelude
import           Node.Editor.React.Model.Node        (ExpressionNode, NodeLoc (NodeLoc), NodePath, inPortAt, nodeLoc)
import           Node.Editor.React.Model.Port        (Mode (Invisible), ensureVisibility, mode)
import           Node.Editor.State.Global            (State)


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
    selfPortVis <- shouldDisplayPortSelf node
    let selfMode = if selfPortVis then ensureVisibility else const Invisible
        node' = node & inPortAt [Self] . mode %~ selfMode
    NodeEditor.addExpressionNode node'
    focusNode $ node ^. nodeLoc
