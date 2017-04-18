module Luna.Studio.Action.Basic.UpdateNode where

import           Control.Monad                               (filterM)
import           Empire.API.Data.Node                        (NodeTypecheckerUpdate, tcNodeId)
import qualified Empire.API.Data.Node                        as Empire
import           Empire.API.Data.Port                        (InPortIndex (Self))
import           Luna.Studio.Action.Basic.DrawConnection     (redrawConnectionsForNode)
import           Luna.Studio.Action.Basic.Scene              (updateScene)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.Model              (shouldDisplayPortSelf)
import qualified Luna.Studio.Action.State.NodeEditor         as NodeEditor
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node                (ExpressionNode, InputNode, NodeLoc, NodePath, OutputNode, inPortAt, nodeLoc)
import           Luna.Studio.React.Model.Node.ExpressionNode (isSelected)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as ExpressionNode
import qualified Luna.Studio.React.Model.Node.SidebarNode    as SidebarNode
import           Luna.Studio.React.Model.Port                (Mode (Invisible), ensureVisibility, mode)
import           Luna.Studio.State.Global                    (State)


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

localUpdateOutputNode :: OutputNode -> Command State Bool
localUpdateOutputNode node = NodeEditor.getOutputNode (node ^. nodeLoc) >>= \case
    Nothing       -> return False
    Just prevNode -> do
        let sidebarMode = prevNode ^. SidebarNode.mode
        NodeEditor.addOutputNode $ node & SidebarNode.mode .~ sidebarMode
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
        void . redrawConnectionsForNode $ node ^. nodeLoc
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
    void $ redrawConnectionsForNode nl

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
