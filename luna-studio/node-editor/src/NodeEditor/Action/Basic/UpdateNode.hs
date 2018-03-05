{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Basic.UpdateNode where

import           Common.Action.Command                       (Command)
import           Common.Prelude
import qualified Data.Map                                    as Map
import           Data.Set                                    (Set)
import qualified Data.Set                                    as Set
import           JS.Visualizers                              (sendInternalData)
import           LunaStudio.Data.Node                        (NodeTypecheckerUpdate, tcNodeId)
import qualified LunaStudio.Data.Node                        as Empire
import           NodeEditor.Action.Basic.AddNode             (localAddExpressionNode, localAddInputNode, localAddOutputNode)
import           NodeEditor.Action.Basic.Scene               (updateScene)
import           NodeEditor.Action.Basic.UpdateSearcherHints (localUpdateSearcherHintsPreservingSelection)
import           NodeEditor.Action.State.Model               (calculatePortSelfMode)
import qualified NodeEditor.Action.State.NodeEditor          as NodeEditor
import           NodeEditor.React.Model.Node                 (ExpressionNode, InputNode, NodePath, OutputNode, inPortAt, nodeLoc)
import           NodeEditor.React.Model.Node.ExpressionNode  (inPortsList, isSelected, nodeType, value, _Error)
import qualified NodeEditor.React.Model.Node.ExpressionNode  as ExpressionNode
import qualified NodeEditor.React.Model.Node.SidebarNode     as SidebarNode
import           NodeEditor.React.Model.NodeEditor           (VisualizationBackup (MessageBackup))
import           NodeEditor.React.Model.Port                 (isSelf, mode, portId)
import qualified NodeEditor.React.Model.Searcher             as Searcher
import           NodeEditor.React.Model.Visualization        (awaitingDataMsg, noVisMsg, visualizers)
import           NodeEditor.State.Global                     (State)


data NodeUpdateModification = KeepPorts
                            | KeepNodeMeta
                            deriving (Eq, Ord)


localUpdateExpressionNodes :: Set NodeUpdateModification -> [ExpressionNode] -> Command State ()
localUpdateExpressionNodes mods = mapM_ (localUpdateExpressionNode mods)

localUpdateInputNode :: InputNode -> Command State Bool
localUpdateInputNode node = NodeEditor.getInputNode (node ^. nodeLoc) >>= \case
    Nothing       -> return False
    Just prevNode -> do
        let sidebarMode = prevNode ^. SidebarNode.mode
        NodeEditor.addInputNode $ node & SidebarNode.mode .~ sidebarMode
        updateScene
        return True

localUpdateOrAddInputNode :: InputNode -> Command State ()
localUpdateOrAddInputNode node = unlessM (localUpdateInputNode node) $ localAddInputNode node

localUpdateOutputNode :: OutputNode -> Command State Bool
localUpdateOutputNode node = NodeEditor.getOutputNode (node ^. nodeLoc) >>= \case
    Nothing       -> return False
    Just prevNode -> do
        let sidebarMode = prevNode ^. SidebarNode.mode
        NodeEditor.addOutputNode $ node & SidebarNode.mode .~ sidebarMode
        updateScene
        return True

localUpdateOrAddOutputNode :: OutputNode -> Command State ()
localUpdateOrAddOutputNode node = unlessM (localUpdateOutputNode node) $ localAddOutputNode node

localUpdateOrAddExpressionNode :: Set NodeUpdateModification -> ExpressionNode -> Command State ()
localUpdateOrAddExpressionNode mods node = unlessM (localUpdateExpressionNode mods node) $ localAddExpressionNode node

localUpdateExpressionNode :: Set NodeUpdateModification -> ExpressionNode -> Command State Bool
localUpdateExpressionNode mods node = NodeEditor.getExpressionNode (node ^. nodeLoc) >>= \case
    Nothing       -> return False
    Just prevNode -> do
        let selected        = prevNode ^. isSelected
            mode'           = prevNode ^. ExpressionNode.mode
            errVis          = prevNode ^. ExpressionNode.errorVisEnabled
            preventPorts    = Set.member KeepPorts    mods
            preventNodeMeta = Set.member KeepNodeMeta mods
            inPorts         = if preventPorts    then prevNode ^. ExpressionNode.inPorts           else node ^. ExpressionNode.inPorts
            outPorts        = if preventPorts    then prevNode ^. ExpressionNode.outPorts          else node ^. ExpressionNode.outPorts
            position        = if preventNodeMeta then prevNode ^. ExpressionNode.position          else node ^. ExpressionNode.position
            visEnabled      = if preventNodeMeta then prevNode ^. ExpressionNode.visEnabled        else node ^. ExpressionNode.visEnabled
            defVis          = if preventNodeMeta then prevNode ^. ExpressionNode.defaultVisualizer else node ^. ExpressionNode.defaultVisualizer
            n               = node & isSelected                       .~ selected
                                   & ExpressionNode.mode              .~ mode'
                                   & ExpressionNode.inPorts           .~ inPorts
                                   & ExpressionNode.outPorts          .~ outPorts
                                   & ExpressionNode.position          .~ position
                                   & ExpressionNode.visEnabled        .~ visEnabled
                                   & ExpressionNode.defaultVisualizer .~ defVis
                                   & ExpressionNode.errorVisEnabled   .~ errVis
            mayPortSelfId = find isSelf . map (view portId) $ inPortsList n
            updatePortSelfMode n' selfPid m = n' & inPortAt selfPid . mode .~ m
        updatedNode <- maybe (return n) (\sPid -> updatePortSelfMode n sPid <$> calculatePortSelfMode n) mayPortSelfId
        NodeEditor.addExpressionNode updatedNode
        updateSearcherClassName updatedNode
        when (prevNode ^. ExpressionNode.nodeType /= updatedNode ^. ExpressionNode.nodeType) $ do
            let nl = updatedNode ^. ExpressionNode.nodeLoc
            NodeEditor.updateNodeVisualizers nl
            hasType       <- isJust <$> NodeEditor.getExpressionNodeType nl
            noVisualizers <- maybe True (Map.null . view visualizers) <$> NodeEditor.getNodeVisualizations nl
            let msg = if hasType && noVisualizers then noVisMsg else awaitingDataMsg
            NodeEditor.setVisualizationData (node ^. ExpressionNode.nodeLoc) (MessageBackup msg) True
        return True

localUpdateNodeTypecheck :: NodePath -> NodeTypecheckerUpdate -> Command State ()
localUpdateNodeTypecheck path update = do
    let nl = convert (path, update ^. tcNodeId)
    case update of
        Empire.ExpressionUpdate _ inPorts outPorts -> do
            withJustM (NodeEditor.getExpressionNode nl) $ \node -> void . localUpdateExpressionNode def $
                node & ExpressionNode.inPorts  .~ convert `fmap` inPorts
                     & ExpressionNode.outPorts .~ convert `fmap` outPorts
                     & ExpressionNode.value    .~ ExpressionNode.AwaitingData
        Empire.OutputSidebarUpdate _ inPorts -> NodeEditor.modifyOutputNode nl $
            SidebarNode.outputSidebarPorts .= convert `fmap` inPorts
        Empire.InputSidebarUpdate _ outPorts -> NodeEditor.modifyInputNode nl $
            SidebarNode.inputSidebarPorts .= convert `fmap2` outPorts

updateSearcherClassName :: ExpressionNode -> Command State ()
updateSearcherClassName node = do
    let (className, _) = Searcher.getPredInfo node
        isNodePred n s = s ^. Searcher.predNl == Just (n ^. ExpressionNode.nodeLoc)
    whenM (maybe False (isNodePred node) <$> NodeEditor.getSearcher) $ do
        NodeEditor.modifySearcher $ Searcher.mode . Searcher._Node . _2 . Searcher.className .= className
        localUpdateSearcherHintsPreservingSelection
