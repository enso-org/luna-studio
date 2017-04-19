module Node.Editor.Action.Basic.UpdateNode where

import           Control.Monad                               (filterM)
import           Empire.API.Data.Node                        (NodeTypecheckerUpdate, tcNodeId)
import qualified Empire.API.Data.Node                        as Empire
import           Empire.API.Data.Port                        (InPortIndex (Self))
import           Node.Editor.Action.Basic.Scene              (updateScene)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.State.Model              (shouldDisplayPortSelf)
import qualified Node.Editor.Action.State.NodeEditor         as NodeEditor
import           Luna.Prelude
import           Node.Editor.React.Model.Node                (ExpressionNode, InputNode, NodeLoc, NodePath, OutputNode, inPortAt, nodeLoc)
import           Node.Editor.React.Model.Node.ExpressionNode (isSelected)
import qualified Node.Editor.React.Model.Node.ExpressionNode as ExpressionNode
import qualified Node.Editor.React.Model.Node.SidebarNode    as SidebarNode
import           Node.Editor.React.Model.Port                (Mode (Invisible), ensureVisibility, mode)
import           Node.Editor.State.Global                    (State)


localUpdateExpressionNodes :: [ExpressionNode] -> Command State ()
localUpdateExpressionNodes = mapM_ localUpdateExpressionNode

localUpdateInputNode :: InputNode -> Command State Bool
localUpdateInputNode node = NodeEditor.getInputNode (node ^. nodeLoc) >>= \case
    Nothing       -> return False
    Just prevNode -> do
        let sidebarMode  = prevNode ^. SidebarNode.mode
            frozenHeight = prevNode ^. SidebarNode.inputFrozenHeight
        NodeEditor.addInputNode $ node & SidebarNode.mode              .~ sidebarMode
                                       & SidebarNode.inputFrozenHeight .~ frozenHeight
        updateScene
        return True

localUpdateOutputNode :: OutputNode -> Command State Bool
localUpdateOutputNode node = NodeEditor.getOutputNode (node ^. nodeLoc) >>= \case
    Nothing       -> return False
    Just prevNode -> do
        let sidebarMode  = prevNode ^. SidebarNode.mode
            frozenHeight = prevNode ^. SidebarNode.outputFrozenHeight
        NodeEditor.addOutputNode $ node & SidebarNode.mode .~ sidebarMode
                                       & SidebarNode.outputFrozenHeight .~ frozenHeight
        updateScene
        return True

localUpdateExpressionNode :: ExpressionNode -> Command State Bool
localUpdateExpressionNode node = NodeEditor.getExpressionNode (node ^. nodeLoc) >>= \case
    Nothing       -> return False
    Just prevNode -> do
        let selected = prevNode ^. isSelected
            mode'    = prevNode ^. ExpressionNode.mode
        portSelfVis <- shouldDisplayPortSelf node
        let (selfMode :: Mode -> Mode) = if portSelfVis then ensureVisibility else const Invisible
        NodeEditor.addExpressionNode $ node & isSelected                      .~ selected
                                            & inPortAt [Self] . mode %~ selfMode
                                            & ExpressionNode.mode             .~ mode'
        return True

localUpdateNodeTypecheck :: NodePath -> NodeTypecheckerUpdate -> Command State ()
localUpdateNodeTypecheck path update = do
    let nl = convert (path, update ^. tcNodeId)
    case update of
        Empire.ExpressionUpdate _ inPorts outPorts -> NodeEditor.modifyExpressionNode nl $ do
            ExpressionNode.inPorts  .= convert `fmap` inPorts
            ExpressionNode.outPorts .= convert `fmap` outPorts
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
