--TODO[PM]: Refactor and treat this as Action
module NodeEditor.Action.Node.EditName where

import           Empire.API.Data.NodeLoc                     (NodeLoc)
import           NodeEditor.Action.Basic                    (renameNode)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.App                (renderIfNeeded)
import           NodeEditor.Action.State.NodeEditor         (modifyExpressionNode)
import           Common.Prelude
import           NodeEditor.React.Model.Node.ExpressionNode (isNameEdited)
import           NodeEditor.React.View.App                  (focus)
import           NodeEditor.React.View.ExpressionNode       (focusNameLabel)
import           NodeEditor.State.Global                    (State)


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
