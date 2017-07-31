module NodeEditor.Action.State.App where

import           Common.Prelude                     hiding (lens)
import           Control.Lens.Internal.Zoom         (Focusing)
import qualified Control.Monad.State                as M
import           NodeEditor.Action.Command          (Command)
import           NodeEditor.React.Model.App         (App, breadcrumbs)
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumb, BreadcrumbItem, Named)
import           NodeEditor.React.Store             (Ref, commit, continueModify)
import qualified NodeEditor.React.Store             as Store
import           NodeEditor.State.Global            (State, ui)
import           NodeEditor.State.UI                (app, renderNeeded)


withApp :: (Ref App -> Command State r) -> Command State r
withApp action = use (ui . app) >>= action

modify :: LensLike' (Focusing Identity b) App s -> M.StateT s Identity b -> Command State b
modify lens action = do
    ui . renderNeeded .= True
    withApp . continueModify $ zoom lens action

get :: Getting r App r -> Command State r
get lens = withApp $ return . view lens <=< Store.get

modifyApp :: M.State App r -> Command State r
modifyApp action = do
    ui . renderNeeded .= True
    withApp $ continueModify action

renderIfNeeded :: Command State ()
renderIfNeeded = whenM (use $ ui . renderNeeded) $ timeIt "render" $ do
    withApp commit
    ui . renderNeeded .= False

setBreadcrumbs :: Breadcrumb (Named BreadcrumbItem)-> Command State ()
setBreadcrumbs bc = modifyApp $ breadcrumbs .= bc
