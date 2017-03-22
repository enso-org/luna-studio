--TODO[PM]: Refactor and treat this as Action
module Luna.Studio.Action.Node.EditName where

import           Empire.API.Data.Node                             (NodeId)
import           Luna.Studio.Action.Basic                         (renameNode)
import           Luna.Studio.Action.Command                       (Command)
import           Luna.Studio.Action.State.App                     (renderIfNeeded)
import           Luna.Studio.Action.State.NodeEditor              (getExpressionNode, modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode      (name, nameEdit)
import           Luna.Studio.React.View.App                       (focus)
import           Luna.Studio.React.View.ExpressionNode.Properties (focusNameLabel)
import           Luna.Studio.State.Global                         (State)


startEditName :: NodeId -> Command State ()
startEditName nid = do
    modifyExpressionNode nid $ do
        name' <- use name
        nameEdit ?= name'
    renderIfNeeded
    liftIO focusNameLabel

editName :: NodeId -> Text -> Command State ()
editName nid update =
    modifyExpressionNode nid $ nameEdit ?= update

applyName :: NodeId -> Command State ()
applyName nid = do
    mayName <- getExpressionNode nid >>= return . maybe Nothing (view nameEdit)
    withJust mayName $ renameNode nid
    liftIO focus

discardName :: NodeId -> Command State ()
discardName nid = do
    modifyExpressionNode nid $ nameEdit .= Nothing
    liftIO focus
