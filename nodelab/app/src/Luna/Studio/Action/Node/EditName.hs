--TODO[PM]: Refactor and treat this as Action
module Luna.Studio.Action.Node.EditName where

import           Empire.API.Data.NodeLoc                     (NodeLoc)
import           Luna.Studio.Action.Basic                    (renameNode)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.App                (renderIfNeeded)
import           Luna.Studio.Action.State.NodeEditor         (modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (isNameEdited)
import           Luna.Studio.React.View.App                  (focus)
import           Luna.Studio.React.View.ExpressionNode       (focusNameLabel)
import           Luna.Studio.State.Global                    (State)


startEditName :: NodeLoc -> Command State ()
startEditName nl = do
    modifyExpressionNode nl $ isNameEdited .= True
    renderIfNeeded
    liftIO focusNameLabel

applyName :: NodeLoc -> Text -> Command State ()
applyName nl newName = do
    renameNode nl newName
    liftIO focus

discardName :: NodeLoc -> Command State ()
discardName nl = do
    modifyExpressionNode nl $ isNameEdited .= False
    liftIO focus
