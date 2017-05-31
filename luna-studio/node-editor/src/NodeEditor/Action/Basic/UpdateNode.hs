module NodeEditor.Action.Basic.UpdateNode where

import           Common.Prelude
import           Control.Monad                              (filterM)
import qualified Data.Map                                   as Map
import           LunaStudio.Data.Node                       (NodeTypecheckerUpdate, tcNodeId)
import qualified LunaStudio.Data.Node                       as Empire
import           LunaStudio.Data.NodeValue                  (applyType)
import           LunaStudio.Data.Port                       (InPortIndex (Self))
import           NodeEditor.Action.Basic.AddNode            (localAddExpressionNode, localAddInputNode, localAddOutputNode)
import           NodeEditor.Action.Basic.Scene              (updateScene)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.Model              (shouldDisplayPortSelf)
import qualified NodeEditor.Action.State.NodeEditor         as NodeEditor
import           NodeEditor.React.Model.Node                (ExpressionNode, InputNode, NodeLoc, NodePath, OutputNode, inPortAt, nodeLoc,
                                                             outPortAt)
import           NodeEditor.React.Model.Node.ExpressionNode (isSelected)
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import qualified NodeEditor.React.Model.Node.SidebarNode    as SidebarNode
import           NodeEditor.React.Model.Port                (Mode (Invisible), ensureVisibility, mode, valueType)
import           NodeEditor.State.Global                    (State)
import qualified NodeEditor.State.Global                    as Global


localUpdateExpressionNodes :: [ExpressionNode] -> Command State ()
localUpdateExpressionNodes = mapM_ localUpdateExpressionNode

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

localUpdateExpressionNode :: ExpressionNode -> Command State Bool
localUpdateExpressionNode = localUpdateExpressionNode' False

localUpdateExpressionNodePreventingPorts :: ExpressionNode -> Command State Bool
localUpdateExpressionNodePreventingPorts = localUpdateExpressionNode' True

localUpdateExpressionNode' :: Bool -> ExpressionNode -> Command State Bool
localUpdateExpressionNode' preventPorts node = NodeEditor.getExpressionNode (node ^. nodeLoc) >>= \case
    Nothing       -> return False
    Just prevNode -> do
        visualizers <- case node ^? outPortAt [] . valueType of
            Nothing  -> return def
            Just tpe -> use Global.visualizers >>= applyType tpe
        portSelfVis <- shouldDisplayPortSelf node
        let selected    = prevNode ^. isSelected
            mode'       = prevNode ^. ExpressionNode.mode
            inPorts     = if preventPorts then prevNode ^. ExpressionNode.inPorts  else node ^. ExpressionNode.inPorts
            outPorts    = if preventPorts then prevNode ^. ExpressionNode.outPorts else node ^. ExpressionNode.outPorts
            (selfMode :: Mode -> Mode) = if portSelfVis then ensureVisibility else const Invisible
            activeVisualizer = case node ^. ExpressionNode.visualization . ExpressionNode.activeVisualizer of
                Nothing               -> listToMaybe (Map.toList visualizers)
                Just v@(vname, vpath) -> if Map.lookup vname visualizers == Just vpath then Just v else listToMaybe (Map.toList visualizers)
        NodeEditor.addExpressionNode $ node & isSelected                   .~ selected
                                            & ExpressionNode.mode          .~ mode'
                                            & ExpressionNode.inPorts       .~ inPorts
                                            & ExpressionNode.outPorts      .~ outPorts
                                            & ExpressionNode.visualization . ExpressionNode.activeVisualizer .~ activeVisualizer
                                            & ExpressionNode.visualization . ExpressionNode.visualizers      .~ visualizers
                                            & inPortAt [Self] . mode       %~ selfMode
        return True

localUpdateOrAddExpressionNode :: ExpressionNode -> Command State ()
localUpdateOrAddExpressionNode node = unlessM (localUpdateExpressionNode node) $ localAddExpressionNode node

localUpdateOrAddExpressionNodePreventingPorts :: ExpressionNode -> Command State ()
localUpdateOrAddExpressionNodePreventingPorts node = unlessM (localUpdateExpressionNodePreventingPorts node) $ localAddExpressionNode node

localUpdateNodeTypecheck :: NodePath -> NodeTypecheckerUpdate -> Command State ()
localUpdateNodeTypecheck path update = do
    let nl = convert (path, update ^. tcNodeId)
    case update of
        Empire.ExpressionUpdate _ inPorts outPorts ->
            withJustM (NodeEditor.getExpressionNode nl) $ \node -> void . localUpdateExpressionNode $
                node & ExpressionNode.inPorts  .~ convert `fmap` inPorts
                     & ExpressionNode.outPorts .~ convert `fmap` outPorts
        Empire.OutputSidebarUpdate _ inPorts -> NodeEditor.modifyOutputNode nl $
            SidebarNode.outputSidebarPorts .= convert `fmap` inPorts
        Empire.InputSidebarUpdate _ outPorts -> NodeEditor.modifyInputNode nl $
            SidebarNode.inputSidebarPorts .= convert `fmap2` outPorts

updateAllPortsSelfVisibility :: Command State ()
updateAllPortsSelfVisibility = do
    nls <- map (view nodeLoc) <$> NodeEditor.getExpressionNodes
    void $ updatePortSelfVisibilityForIds nls

updatePortSelfVisibilityForIds :: [NodeLoc] -> Command State [NodeLoc]
updatePortSelfVisibilityForIds = filterM updatePortSelfVisibility

updatePortSelfVisibility :: NodeLoc -> Command State Bool
updatePortSelfVisibility nid = NodeEditor.getExpressionNode nid >>=
    maybe (return False) ( \node -> do
        vis <- shouldDisplayPortSelf node
        NodeEditor.modifyExpressionNode nid $ inPortAt [Self] . mode %= if vis then ensureVisibility else const Invisible
        return True
        )
