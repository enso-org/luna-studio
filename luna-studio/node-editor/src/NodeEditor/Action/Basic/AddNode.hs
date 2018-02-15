{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Basic.AddNode where

import           Common.Action.Command                   (Command)
import           Common.Prelude
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import           JS.Visualizers                          (sendInternalData)
import           LunaStudio.Data.Geometry                (snap)
import           LunaStudio.Data.LabeledTree             (LabeledTree (LabeledTree))
import qualified LunaStudio.Data.Node                    as Empire
import           LunaStudio.Data.NodeMeta                (NodeMeta (NodeMeta))
import           LunaStudio.Data.Port                    (InPortIndex (Arg), Port (Port), PortState (NotConnected))
import           LunaStudio.Data.Position                (Position)
import           LunaStudio.Data.TypeRep                 (TypeRep (TStar))
import           NodeEditor.Action.Basic.FocusNode       (focusNode)
import           NodeEditor.Action.Basic.SelectNode      (selectNode)
import           NodeEditor.Action.Basic.UpdateNodeValue (setVisualizationData)
import qualified NodeEditor.Action.Batch                 as Batch
import           NodeEditor.Action.State.Model           (calculatePortSelfMode)
import           NodeEditor.Action.State.NodeEditor      (addInputNode, addOutputNode, getSelectedNodes, updateVisualizationsForNode)
import qualified NodeEditor.Action.State.NodeEditor      as NodeEditor
import           NodeEditor.Action.UUID                  (getUUID)
import           NodeEditor.React.Model.Node             (ExpressionNode, InputNode, NodeLoc (NodeLoc), NodePath, OutputNode, inPortAt,
                                                          inPortsList, nodeLoc)
import           NodeEditor.React.Model.NodeEditor       (VisualizationBackup (MessageBackup))
import           NodeEditor.React.Model.Port             (isSelf, mode, portId)
import           NodeEditor.React.Model.Visualization    (awaitingDataMsg)
import           NodeEditor.State.Global                 (State)


createNode :: NodePath -> Position -> Text -> Bool -> Command State ()
createNode parentPath nodePos expr isDefinition = do
    selected <- getSelectedNodes
    nid      <- getUUID
    let snappedPos  = snap nodePos
        nodeMeta    = NodeMeta snappedPos False def
        connectTo   = if length selected == 1
                      then view nodeLoc <$> listToMaybe selected
                      else Nothing
        defInPorts  = LabeledTree def $ Port [Arg 0] (Text.pack "") TStar NotConnected
        defOutPorts = LabeledTree def $ Port []      (Text.pack "") TStar NotConnected
        empireNode  = Empire.ExpressionNode nid expr isDefinition def def defInPorts defOutPorts nodeMeta False
        node        = convert (parentPath, empireNode)
        nl          = NodeLoc parentPath nid
    localAddExpressionNode node
    selectNode nl
    Batch.addNode nl expr nodeMeta connectTo

localAddExpressionNodes :: [ExpressionNode] -> Command State ()
localAddExpressionNodes = mapM_ localAddExpressionNode

localAddExpressionNode :: ExpressionNode -> Command State ()
localAddExpressionNode node = do
    let mayPortSelfId            = find isSelf . map (view portId) $ inPortsList node
        updatePortSelf selfPid m = node & inPortAt selfPid . mode .~ m
    node' <- maybe (return node) (\selfPid -> updatePortSelf selfPid <$> calculatePortSelfMode node) mayPortSelfId
    NodeEditor.addExpressionNode node'
    setVisualizationData (node ^. nodeLoc) (MessageBackup awaitingDataMsg) True
    focusNode $ node ^. nodeLoc

localAddInputNode :: InputNode -> Command State ()
localAddInputNode = addInputNode

localAddOutputNode :: OutputNode -> Command State ()
localAddOutputNode = addOutputNode
