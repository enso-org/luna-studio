{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Basic.UpdateNode where

import           Common.Action.Command                       (Command)
import           Common.Prelude
import qualified Data.Map                                    as Map
import           Data.Set                                    (Set)
import qualified Data.Set                                    as Set
import           LunaStudio.Data.Node                        (NodeTypecheckerUpdate, tcNodeId)
import qualified LunaStudio.Data.Node                        as API
import           LunaStudio.Data.NodeLoc                     (NodeLoc)
import           NodeEditor.Action.Basic.AddNode             (localAddExpressionNode, localAddInputNode, localAddOutputNode)
import           NodeEditor.Action.Basic.Scene               (updateScene)
import           NodeEditor.Action.Basic.UpdateSearcherHints (localUpdateSearcherHintsPreservingSelection)
import           NodeEditor.Action.State.Model               (calculatePortSelfMode)
import qualified NodeEditor.Action.State.NodeEditor          as NodeEditor
import           NodeEditor.Action.State.Visualization       (adjustToType)
import           NodeEditor.React.Model.Node                 (ExpressionNode, InputNode, NodePath, OutputNode, inPortAt, nodeLoc)
import           NodeEditor.React.Model.Node.ExpressionNode  (inPortsList, isSelected)
import qualified NodeEditor.React.Model.Node.ExpressionNode  as ExpressionNode
import qualified NodeEditor.React.Model.Node.SidebarNode     as SidebarNode
import           NodeEditor.React.Model.Port                 (InPort, InPortTree, OutPort, OutPortTree, isSelf, mode, portId)
import qualified NodeEditor.React.Model.Searcher             as Searcher
import           NodeEditor.State.Global                     (State)


data NodeUpdateModification = KeepPorts
                            | KeepNodeMeta
                            | MergePorts
                            deriving (Eq, Ord)


localUpdateExpressionNodes :: Set NodeUpdateModification -> [ExpressionNode]
    -> Command State ()
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
localUpdateOrAddInputNode node = unlessM (localUpdateInputNode node)
    $ localAddInputNode node

localUpdateOutputNode :: OutputNode -> Command State Bool
localUpdateOutputNode node = NodeEditor.getOutputNode (node ^. nodeLoc)
    >>= \case
        Nothing       -> return False
        Just prevNode -> do
            let sidebarMode = prevNode ^. SidebarNode.mode
            NodeEditor.addOutputNode $ node & SidebarNode.mode .~ sidebarMode
            updateScene
            return True

localUpdateOrAddOutputNode :: OutputNode -> Command State ()
localUpdateOrAddOutputNode node
    = unlessM (localUpdateOutputNode node) $ localAddOutputNode node

localUpdateOrAddExpressionNode :: Set NodeUpdateModification -> ExpressionNode
    -> Command State ()
localUpdateOrAddExpressionNode mods node = unlessM
    (localUpdateExpressionNode mods node)
    $ localAddExpressionNode node

localUpdateExpressionNodeInPorts :: NodeLoc -> InPortTree InPort
    -> Command State ()
localUpdateExpressionNodeInPorts nl ports = NodeEditor.modifyExpressionNode nl
    $ ExpressionNode.inPorts .= ports

localUpdateExpressionNodeOutPorts :: NodeLoc -> OutPortTree OutPort
    -> Command State ()
localUpdateExpressionNodeOutPorts nl ports = NodeEditor.modifyExpressionNode nl
    $ ExpressionNode.outPorts .= ports

localUpdateIsDefinition :: NodeLoc -> Bool -> Command State ()
localUpdateIsDefinition nl update = NodeEditor.modifyExpressionNode nl
    $ ExpressionNode.isDefinition .= update

localUpdateNodeCode :: NodeLoc -> Text -> Command State ()
localUpdateNodeCode nl update = NodeEditor.modifyExpressionNode nl
    $ ExpressionNode.code .= update

localUpdateExpressionNode :: Set NodeUpdateModification -> ExpressionNode
    -> Command State Bool
localUpdateExpressionNode mods node
    = NodeEditor.getExpressionNode (node ^. nodeLoc) >>= \case
        Nothing       -> return False
        Just prevNode -> do
            let selected        = prevNode ^. isSelected
                mode'           = prevNode ^. ExpressionNode.mode
                errVis          = prevNode ^. ExpressionNode.errorVisEnabled
                preventPorts    = Set.member KeepPorts    mods
                mergePorts      = Set.member MergePorts   mods
                preventNodeMeta = Set.member KeepNodeMeta mods
                inPorts         = if preventPorts || (mergePorts && (prevNode ^. ExpressionNode.inPorts /= def))
                    then prevNode ^. ExpressionNode.inPorts
                    else node ^. ExpressionNode.inPorts
                outPorts        = if preventPorts || (mergePorts && (prevNode ^. ExpressionNode.outPorts /= def))
                    then prevNode ^. ExpressionNode.outPorts
                    else node ^. ExpressionNode.outPorts
                position        = if preventNodeMeta
                    then prevNode ^. ExpressionNode.position
                    else node ^. ExpressionNode.position
                visEnabled      = if preventNodeMeta
                    then prevNode ^. ExpressionNode.visEnabled
                    else node ^. ExpressionNode.visEnabled
                defVis          = if preventNodeMeta
                    then prevNode ^. ExpressionNode.defaultVisualizer
                    else node ^. ExpressionNode.defaultVisualizer
                n = node
                    & isSelected                       .~ selected
                    & ExpressionNode.mode              .~ mode'
                    & ExpressionNode.inPorts           .~ inPorts
                    & ExpressionNode.outPorts          .~ outPorts
                    & ExpressionNode.position          .~ position
                    & ExpressionNode.visEnabled        .~ visEnabled
                    & ExpressionNode.defaultVisualizer .~ defVis
                    & ExpressionNode.errorVisEnabled   .~ errVis

                mayPortSelfId = find isSelf . map (view portId) $ inPortsList n
                updatePortSelfMode n' selfPid m
                    = n' & inPortAt selfPid . mode .~ m
            updatedNode <- maybe
                (return n)
                (\sPid -> updatePortSelfMode n sPid <$> calculatePortSelfMode n)
                mayPortSelfId
            NodeEditor.addExpressionNode updatedNode
            updateSearcherClassName updatedNode
            let typeMatch
                    =  prevNode ^. ExpressionNode.nodeType
                    == updatedNode ^. ExpressionNode.nodeType
            unless typeMatch
                . adjustToType $ updatedNode ^. ExpressionNode.nodeLoc
            return True

localUpdateCanEnterExpressionNode :: NodeLoc -> Bool -> Command State ()
localUpdateCanEnterExpressionNode nl update = NodeEditor.modifyExpressionNode nl
    $ ExpressionNode.canEnter .= update

localUpdateNodeTypecheck :: NodePath -> NodeTypecheckerUpdate
    -> Command State ()
localUpdateNodeTypecheck path update = do
    let nl = convert (path, update ^. tcNodeId)
    case update of
        API.ExpressionUpdate _ inPorts outPorts ->
            withJustM (NodeEditor.getExpressionNode nl)
                $ \node -> void . localUpdateExpressionNode def $ node
                    & ExpressionNode.inPorts  .~ convert `fmap` inPorts
                    & ExpressionNode.outPorts .~ convert `fmap` outPorts
                    & ExpressionNode.value    %~ (\value ->
                        if  value == ExpressionNode.AwaitingTypecheck
                            || ExpressionNode.returnsError node
                            then ExpressionNode.AwaitingData else value)
        API.OutputSidebarUpdate _ inPorts -> NodeEditor.modifyOutputNode nl $
            SidebarNode.outputSidebarPorts .= convert `fmap` inPorts
        API.InputSidebarUpdate _ outPorts -> NodeEditor.modifyInputNode nl $
            SidebarNode.inputSidebarPorts .= convert `fmap2` outPorts

updateSearcherClassName :: ExpressionNode -> Command State ()
updateSearcherClassName node = do
    let (className, _) = Searcher.getPredInfo node
        isNodePred n s
            = s ^. Searcher.predNl == Just (n ^. ExpressionNode.nodeLoc)
    whenM (maybe False (isNodePred node) <$> NodeEditor.getSearcher) $ do
        NodeEditor.modifySearcher
            $ Searcher.mode . Searcher._Node . _2 . Searcher.className
                .= className
        localUpdateSearcherHintsPreservingSelection
