module NodeEditor.Action.Basic.DestroyGraph where

import           NodeEditor.Action.Command   (Command)
import           NodeEditor.Action.State.App (modifyApp)
import           NodeEditor.Batch.Workspace  (isGraphLoaded)
import           Common.Prelude
import           NodeEditor.React.Model.App  (codeEditor, nodeEditor)
import           NodeEditor.State.Global     (State, workspace)


destroyGraph :: Command State ()
destroyGraph = do
    workspace . isGraphLoaded .= False
    modifyApp $ do
        codeEditor .= def
        nodeEditor .= def
