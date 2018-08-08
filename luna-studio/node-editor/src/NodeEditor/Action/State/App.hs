module NodeEditor.Action.State.App where

import           Common.Action.Command              (Command)
import           Common.Prelude                     hiding (get, lens)
import           Control.Lens.Internal.Zoom         (Focusing)
import qualified Control.Monad.State                as M
import           NodeEditor.Batch.Workspace         (Workspace)
import           NodeEditor.React.Model.App         (App, breadcrumbs, workspace)
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumb, BreadcrumbItem, Named)
import           NodeEditor.React.Store             (Ref, continueModify)
import qualified NodeEditor.React.Store             as Store
import           NodeEditor.View.App                (appView)
import           NodeEditor.State.Global            (State, ui)
import           NodeEditor.State.UI                (app, oldApp, renderNeeded)


withApp :: (Ref App -> Command State r) -> Command State r
withApp action = use (ui . app) >>= action

modify :: LensLike' (Focusing Identity b) App s -> M.State s b -> Command State b
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
    renderBaseGL
    ui . renderNeeded .= False

setBreadcrumbs :: Breadcrumb (Named BreadcrumbItem) -> Command State ()
setBreadcrumbs bc = modifyApp $ breadcrumbs .= bc

getWorkspace :: Command State (Maybe Workspace)
getWorkspace = get workspace

renderBaseGL :: Command State ()
renderBaseGL = do
    current <- get id
    old <- use $ ui . oldApp
    appView old current
    ui . oldApp .= current