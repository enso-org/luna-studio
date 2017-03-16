module Luna.Studio.Action.State.App where

import           Control.Lens.Internal.Zoom          (Focusing)
import qualified Control.Monad.State                 as M
import qualified JS.GoogleAnalytics                  as GA
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Prelude                 hiding (lens)
import           Luna.Studio.React.Model.App         (App, breadcrumbs, codeEditor)
import           Luna.Studio.React.Model.Breadcrumbs (Breadcrumb, BreadcrumbItem, Named)
import           Luna.Studio.React.Model.CodeEditor  (CodeEditor, visible)
import           Luna.Studio.React.Store             (Ref, commit, continueModify)
import qualified Luna.Studio.React.Store             as Store
import           Luna.Studio.State.Global            (State, app, renderNeeded)


withApp :: (Ref App -> Command State r) -> Command State r
withApp action = use app >>= action

modify :: LensLike' (Focusing Identity b) App s -> M.StateT s Identity b -> Command State b
modify lens action = do
    renderNeeded .= True
    withApp . continueModify $ zoom lens action

get :: Getting r App r -> Command State r
get lens = withApp $ return . view lens <=< Store.get

modifyApp :: M.State App r -> Command State r
modifyApp action = do
    renderNeeded .= True
    withApp $ continueModify action

renderIfNeeded :: Command State ()
renderIfNeeded = whenM (use renderNeeded) $ withApp commit >> renderNeeded .= False

setBreadcrumbs :: Breadcrumb (Named BreadcrumbItem)-> Command State ()
setBreadcrumbs input = modifyApp $ breadcrumbs .= input

modifyCodeEditor :: (CodeEditor -> CodeEditor) -> Command State ()
modifyCodeEditor f = modifyApp $ codeEditor %= f

toggleCodeEditor :: Command State ()
toggleCodeEditor = do
    GA.sendEvent GA.ToggleText
    modifyCodeEditor $ visible %~ not
    -- size <- use $ Global.camera . Camera.camera . Camera.windowSize --TODO[react] remove
    -- Camera.updateWindowSize size
