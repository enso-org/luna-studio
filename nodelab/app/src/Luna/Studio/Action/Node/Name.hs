module Luna.Studio.Action.Node.Name
    ( rename
    , startEditName
    , editName
    , applyName
    , discardName
    ) where

import           Empire.API.Data.Node         (NodeId)
import qualified Empire.API.Data.Node         as Node
import qualified Luna.Studio.Action.Batch     as Batch
import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node as Model
import qualified Luna.Studio.React.View.App   as App
import qualified Luna.Studio.React.View.NodeProperties  as NodeProperties
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.Graph      as Graph


startEditName :: NodeId -> Command State ()
startEditName nodeId = do
    Global.withNode nodeId $ do
        name <- use Model.name
        Model.nameEdit ?= name
    liftIO NodeProperties.focusNameLabel

editName :: NodeId -> Text -> Command State ()
editName nodeId name =
    Global.withNode nodeId $ Model.nameEdit ?= name

applyName :: NodeId -> Command State ()
applyName nodeId = do
    mayName <- Global.withNode nodeId $ do
        mayName <- use Model.nameEdit
        Model.nameEdit .= Nothing
        -- case ne of
            -- Just name -> Model.name .= name
            -- Nothing -> return ()
        return mayName
    withJust mayName $ Batch.renameNode nodeId
    liftIO App.focus

discardName :: NodeId -> Command State ()
discardName nodeId = do
    Global.withNode nodeId $ Model.nameEdit .= Nothing
    liftIO App.focus

rename :: NodeId -> Text -> Command State ()
rename nodeId name = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.name .= name
    Global.withNode nodeId $ Model.name .= name
