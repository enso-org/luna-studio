module NodeEditor.Action.State.App where

import Common.Prelude hiding (get, lens)

import qualified Control.Monad.State    as Monad
import qualified NodeEditor.React.Store as Store

import Common.Action.Command              (Command)
import Control.Lens.Internal.Zoom         (Focusing)
import NodeEditor.Batch.Workspace         (Workspace)
import NodeEditor.React.Model.App         (App, breadcrumbs, workspace)
import NodeEditor.React.Model.Breadcrumbs (Breadcrumb, BreadcrumbItem, Named)
import NodeEditor.React.Store             (Ref, commit, continueModify)
import NodeEditor.State.Global            (State, ui)
import NodeEditor.State.UI                (app, renderNeeded)


withApp :: (Ref App -> Command State r) -> Command State r
withApp action = use (ui . app) >>= action

modify :: LensLike' (Focusing Identity b) App s -> Monad.State s b
       -> Command State b
modify lens action = do
    ui . renderNeeded .= True
    withApp . continueModify $ zoom lens action

get :: Getting r App r -> Command State r
get lens = withApp $ return . view lens <=< Store.get

modifyApp :: Monad.State App r -> Command State r
modifyApp action = do
    ui . renderNeeded .= True
    withApp $ continueModify action

renderIfNeeded :: Command State ()
renderIfNeeded = do
    whenM (use $ ui . renderNeeded) $ timeIt "render" $ do
        withApp commit
        ui . renderNeeded .= False

setBreadcrumbs :: Breadcrumb (Named BreadcrumbItem) -> Command State ()
setBreadcrumbs bc = modifyApp $ breadcrumbs .= bc

getWorkspace :: Command State (Maybe Workspace)
getWorkspace = get workspace
