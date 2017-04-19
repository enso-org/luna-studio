--TODO[PM]: Refactor and treat this as Action
module Node.Editor.Action.Node.EditName where

import           Empire.API.Data.NodeLoc                     (NodeLoc)
import           Node.Editor.Action.Basic                    (renameNode)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.State.App                (renderIfNeeded)
import           Node.Editor.Action.State.NodeEditor         (modifyExpressionNode)
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (isNameEdited)
import           Node.Editor.React.View.App                  (focus)
import           Node.Editor.React.View.ExpressionNode       (focusNameLabel)
import           Node.Editor.State.Global                    (State)


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
