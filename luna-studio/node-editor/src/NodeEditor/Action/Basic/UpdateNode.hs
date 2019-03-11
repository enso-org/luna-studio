module NodeEditor.Action.Basic.UpdateNode where

import Common.Prelude

import qualified Data.Map                                    as Map
import qualified Data.Set                                    as Set
import qualified LunaStudio.Data.Node                        as API
import qualified LunaStudio.Data.PortRef                     as PortRef
import qualified LunaStudio.Data.TypeRep                     as TypeRep
import qualified NodeEditor.Action.State.NodeEditor          as NodeEditor
import qualified NodeEditor.React.Model.Node.ExpressionNode  as ExpressionNode
import qualified NodeEditor.React.Model.Node.SidebarNode     as SidebarNode
import qualified NodeEditor.React.Model.Port                 as Port
import qualified NodeEditor.Action.Basic.UpdateSearcherHints as Searcher

import Common.Action.Command                      (Command)
import Data.Set                                   (Set)
import LunaStudio.Data.Node                       (NodeTypecheckerUpdate,
                                                   tcNodeId)
import LunaStudio.Data.NodeLoc                    (NodeLoc)
import NodeEditor.Action.Basic.AddNode            (localAddExpressionNode,
                                                   localAddInputNode,
                                                   localAddOutputNode)
import NodeEditor.Action.Basic.Scene              (updateScene)
import NodeEditor.Action.State.Model              (calculatePortSelfMode)
import NodeEditor.React.Model.Node                (ExpressionNode, InputNode,
                                                   NodePath, OutputNode,
                                                   inPortAt, nodeLoc)
import NodeEditor.React.Model.Node.ExpressionNode (inPortsList, isSelected)
import NodeEditor.React.Model.Port                (InPort, InPortTree, OutPort,
                                                   OutPortTree, isSelf, mode,
                                                   portId)
import NodeEditor.React.Model.Visualization       (VisualizationBackup (MessageBackup),
                                                   awaitingDataMsg, noVisMsg,
                                                   visualizers)
import NodeEditor.State.Global                    (State)


data NodeUpdateModification
    = KeepPorts
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
            unless typeMatch $ do
                let nl = updatedNode ^. ExpressionNode.nodeLoc
                NodeEditor.updateNodeVisualizers nl
                hasType       <- isJust <$> NodeEditor.getExpressionNodeType nl
                noVisualizers <- maybe
                    True
                    (Map.null . view visualizers)
                    <$> NodeEditor.getNodeVisualizations nl
                let msg = if hasType && noVisualizers
                        then noVisMsg
                        else awaitingDataMsg
                NodeEditor.setVisualizationData
                    (node ^. ExpressionNode.nodeLoc)
                    (MessageBackup msg)
                    True
            return True

localUpdateCanEnterExpressionNode :: NodeLoc -> Bool -> Command State ()
localUpdateCanEnterExpressionNode nl update = NodeEditor.modifyExpressionNode nl
    $ ExpressionNode.canEnter .= update

localUpdateNodeTypecheck :: NodePath -> NodeTypecheckerUpdate -> Command State ()
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
updateSearcherClassName updatedNode = do
    mayConnectedPortRef <- Searcher.getConnectedPortRef
    let mayConnectedNL   = view PortRef.nodeLoc <$> mayConnectedPortRef
        nl               = updatedNode ^. ExpressionNode.nodeLoc
        mayConnectedPort = listToMaybe $ ExpressionNode.outPortsList updatedNode
        className        = toClassName =<< mayConnectedPort
        toClassName p    = convert <$> p ^? Port.valueType . TypeRep._TCons . _1

    when (mayConnectedNL == Just nl) $
        Searcher.updateClassName className
