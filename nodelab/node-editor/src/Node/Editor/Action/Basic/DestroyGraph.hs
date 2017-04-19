module Node.Editor.Action.Basic.DestroyGraph where

import           Node.Editor.Action.Command   (Command)
import           Node.Editor.Action.State.App (modifyApp)
import           Node.Editor.Batch.Workspace  (isGraphLoaded)
import           Luna.Prelude
import           Node.Editor.React.Model.App  (codeEditor, nodeEditor)
import           Node.Editor.State.Global     (State, workspace)


destroyGraph :: Command State ()
destroyGraph = do
    workspace . isGraphLoaded .= False
    modifyApp $ do
        codeEditor .= def
        nodeEditor .= def
