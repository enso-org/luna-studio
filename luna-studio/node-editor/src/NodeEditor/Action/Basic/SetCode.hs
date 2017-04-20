module NodeEditor.Action.Basic.SetCode where

import           NodeEditor.Action.Command         (Command)
import           NodeEditor.Action.State.App       (modifyCodeEditor)
import           Common.Prelude
import           NodeEditor.React.Model.CodeEditor (code)
import           NodeEditor.State.Global           (State)


localSetCode :: Text -> Command State ()
localSetCode input = do
    -- liftIO . pushCode . convert . toString $ input
    modifyCodeEditor $ code .~ input
