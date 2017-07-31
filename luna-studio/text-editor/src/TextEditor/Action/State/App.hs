module TextEditor.Action.State.App where

import           Control.Lens.Internal.Zoom          (Focusing)
import qualified Control.Monad.State                 as M
import           Common.Action.Command          (Command)
import           Common.Prelude                 hiding (lens)
import           TextEditor.React.Model.CodeEditor  (CodeEditor, visible)
import           TextEditor.State.Global            (State, app)


withApp :: (Ref App -> Command State r) -> Command State r
withApp action = use app >>= action

modify :: LensLike' (Focusing Identity b) App s -> M.StateT s Identity b -> Command State b
modify lens action = do
    withApp . continueModify $ zoom lens action

get :: Getting r App r -> Command State r
get lens = withApp $ return . view lens <=< Store.get

modifyApp :: M.State App r -> Command State r
modifyApp action = do
    withApp $ continueModify action
