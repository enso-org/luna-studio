module Luna.Studio.Action.Basic.DestroyGraph where

import           Luna.Studio.Action.Command   (Command)
import           Luna.Studio.Action.State.App (modifyApp)
import           Luna.Studio.Batch.Workspace  (isGraphLoaded)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App  (codeEditor, nodeEditor)
import           Luna.Studio.State.Global     (State, workspace)


destroyGraph :: Command State ()
destroyGraph = do
    workspace . isGraphLoaded .= False
    modifyApp $ do
        codeEditor .= def
        nodeEditor .= def
