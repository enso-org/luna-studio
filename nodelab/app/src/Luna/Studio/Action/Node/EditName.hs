--TODO[PM]: Refactor and treat this as Action
module Luna.Studio.Action.Node.EditName where

import           Empire.API.Data.NodeLoc                          (NodeLoc)
import           Luna.Studio.Action.Basic                         (renameNode)
import           Luna.Studio.Action.Command                       (Command)
import           Luna.Studio.Action.State.App                     (renderIfNeeded)
import           Luna.Studio.Action.State.NodeEditor              (getExpressionNode, modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode      (name, nameEdit)
import           Luna.Studio.React.View.App                       (focus)
import           Luna.Studio.React.View.ExpressionNode.Properties (focusNameLabel)
import           Luna.Studio.State.Global                         (State)


startEditName :: NodeLoc -> Command State ()
startEditName nl = do
    modifyExpressionNode nl $ do
        name' <- use name
        nameEdit ?= name'
    renderIfNeeded
    liftIO focusNameLabel

editName :: NodeLoc -> Text -> Command State ()
editName nl update =
    modifyExpressionNode nl $ nameEdit ?= update

applyName :: NodeLoc -> Command State ()
applyName nl = do
    mayName <- getExpressionNode nl >>= return . maybe Nothing (view nameEdit)
    withJust mayName $ renameNode nl
    liftIO focus

discardName :: NodeLoc -> Command State ()
discardName nl = do
    modifyExpressionNode nl $ nameEdit .= Nothing
    liftIO focus
