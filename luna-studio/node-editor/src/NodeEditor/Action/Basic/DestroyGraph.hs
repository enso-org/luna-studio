module NodeEditor.Action.Basic.DestroyGraph where

import           Common.Prelude
import           NodeEditor.Action.Command   (Command)
import           NodeEditor.Action.State.App (modifyApp)
import           NodeEditor.Batch.Workspace  (isGraphLoaded)
import           NodeEditor.React.Model.App  (nodeEditor)
import           NodeEditor.State.Global     (State, workspace)


destroyGraph :: Command State ()
destroyGraph = do
    workspace . isGraphLoaded .= False
    modifyApp $ do
        nodeEditor .= def
