module Luna.Studio.Action.Basic.SetCode where

<<<<<<< HEAD
import           JS.Atom                            (pushCode)
=======
>>>>>>> atomdoublerepr
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.State.App       (modifyCodeEditor)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.CodeEditor (code)
import           Luna.Studio.State.Global           (State)


localSetCode :: Text -> Command State ()
localSetCode input = do
    -- liftIO . pushCode . convert . toString $ input
    modifyCodeEditor $ code .~ input
