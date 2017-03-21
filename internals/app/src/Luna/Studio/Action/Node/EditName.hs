--TODO[PM]: Refactor and treat this as Action
module Luna.Studio.Action.Node.EditName where

import           Empire.API.Data.Node                   (NodeId)
import           Luna.Studio.Action.Basic               (renameNode)
import           Luna.Studio.Action.Command             (Command)
import           Luna.Studio.Action.State.App           (renderIfNeeded)
import           Luna.Studio.Action.State.NodeEditor    (getNode, modifyNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node           (name, nameEdit)
import           Luna.Studio.React.View.App             (focus)
import           Luna.Studio.React.View.Node.Properties (focusNameLabel)
import           Luna.Studio.State.Global               (State)


startEditName :: NodeId -> Command State ()
startEditName nid = do
    modifyNode nid $ do
        name' <- use name
        nameEdit ?= name'
    renderIfNeeded
    liftIO focusNameLabel

editName :: NodeId -> Text -> Command State ()
editName nid update =
    modifyNode nid $ nameEdit ?= update

applyName :: NodeId -> Command State ()
applyName nid = do
    mayName <- getNode nid >>= return . maybe Nothing (view nameEdit)
    withJust mayName $ renameNode nid
    liftIO focus

discardName :: NodeId -> Command State ()
discardName nid = do
    modifyNode nid $ nameEdit .= Nothing
    liftIO focus
