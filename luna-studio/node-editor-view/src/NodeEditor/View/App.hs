module NodeEditor.View.App where

import           Common.Prelude
import           NodeEditor.React.Model.App  (App, breadcrumbs, nodeEditor)
import           NodeEditor.View.Breadcrumbs (breadcrumbsView)
import           NodeEditor.View.Diff        (diff, runDiffT)
import           NodeEditor.View.NodeEditor  (nodeEditorView)


appView :: MonadIO m => App -> App -> m ()
appView = runDiffT $ do
    diff breadcrumbsView $ to convert
    diff nodeEditorView  nodeEditor
