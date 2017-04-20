module NodeEditor.Action.State.App where

import           Control.Lens.Internal.Zoom          (Focusing)
import qualified Control.Monad.State                 as M
import qualified JS.GoogleAnalytics                  as GA
import           NodeEditor.Action.Command          (Command)
import           Common.Prelude                 hiding (lens)
import           NodeEditor.React.Model.App         (App, breadcrumbs, codeEditor)
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumb, BreadcrumbItem, Named)
import           NodeEditor.React.Model.CodeEditor  (CodeEditor, visible)
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
renderIfNeeded = whenM (use $ ui . renderNeeded) $ do
    withApp commit
    ui . renderNeeded .= False

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
