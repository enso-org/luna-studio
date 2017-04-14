module Internal.Action.State.App where

import           Control.Lens.Internal.Zoom          (Focusing)
import qualified Control.Monad.State                 as M
import           Internal.Action.Command          (Command)
import           Internal.Prelude                 hiding (lens)
-- import           Internal.React.Model.App         (App, breadcrumbs, codeEditor)
-- import           Internal.React.Model.Breadcrumbs (Breadcrumb, BreadcrumbItem, Named)
import           Internal.React.Model.CodeEditor  (CodeEditor, visible)
-- import           Internal.React.Store             (Ref, commit, continueModify)
-- import qualified Internal.React.Store             as Store
import           Internal.State.Global            (State, app)


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
