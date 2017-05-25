module NodeEditor.Action.Basic.AddNode where

import           Common.Prelude
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           LunaStudio.Data.LabeledTree        (LabeledTree (LabeledTree))
import qualified LunaStudio.Data.Node               as Empire
import           LunaStudio.Data.NodeMeta           (NodeMeta (NodeMeta))
import           LunaStudio.Data.Port               (InPortIndex (Arg, Self), Port (Port), PortState (NotConnected))
import           LunaStudio.Data.Position           (Position)
import           LunaStudio.Data.TypeRep            (TypeRep (TStar))
import qualified JS.GoogleAnalytics                 as GA
import           NodeEditor.Action.Basic.FocusNode  (focusNode)
import           NodeEditor.Action.Basic.SelectNode (selectNode)
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.Command          (Command)
import           LunaStudio.Data.Geometry        (snap)
import           NodeEditor.Action.State.Model      (shouldDisplayPortSelf)
import           NodeEditor.Action.State.NodeEditor (getSelectedNodes)
import           NodeEditor.Action.State.NodeEditor (addInputNode, addOutputNode)
import qualified NodeEditor.Action.State.NodeEditor as NodeEditor
import           NodeEditor.Action.UUID             (getUUID)
import           NodeEditor.React.Model.Node        (ExpressionNode, InputNode, NodeLoc (NodeLoc), NodePath, OutputNode, inPortAt, nodeLoc)
import           NodeEditor.React.Model.Port        (Mode (Invisible), ensureVisibility, mode)
import           NodeEditor.State.Global            (State)


createNode :: NodePath -> Position -> Text -> Command State ()
createNode parentPath nodePos expr = do
    selected <- getSelectedNodes
    nid      <- getUUID
    let snappedPos  = snap nodePos
        nodeMeta    = NodeMeta snappedPos True
        connectTo   = if length selected == 1
                      then view nodeLoc <$> listToMaybe selected
                      else Nothing
        defInPorts  = LabeledTree def $ Port [Arg 0] (Text.pack "") TStar NotConnected
        defOutPorts = LabeledTree def $ Port []      (Text.pack "") TStar NotConnected
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

localAddInputNode :: InputNode -> Command State ()
localAddInputNode = addInputNode

localAddOutputNode :: OutputNode -> Command State ()
localAddOutputNode = addOutputNode