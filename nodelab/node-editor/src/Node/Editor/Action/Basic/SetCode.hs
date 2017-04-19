module Node.Editor.Action.Basic.SetCode where

import           Node.Editor.Action.Command         (Command)
import           Node.Editor.Action.State.App       (modifyCodeEditor)
import           Luna.Prelude
import           Node.Editor.React.Model.CodeEditor (code)
import           Node.Editor.State.Global           (State)


localSetCode :: Text -> Command State ()
localSetCode input = do
    -- liftIO . pushCode . convert . toString $ input
    modifyCodeEditor $ code .~ input
