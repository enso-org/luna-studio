{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.View.Breadcrumbs where

import           Common.Data.JSON                    (toJSONVal)
import           Common.Prelude
import           NodeEditor.React.Model.Breadcrumbs  (Breadcrumbs)
import           NodeEditor.View.Diff                (DiffT, diffApply)


foreign import javascript safe "atomCallback.getNodeEditorView().setBreadcrumbs($1)"
    setBreadcrumbs__ :: JSVal -> IO ()

setBreadcrumbs :: MonadIO m => Breadcrumbs -> m ()
setBreadcrumbs = liftIO . setBreadcrumbs__ <=< toJSONVal

breadcrumbsView :: MonadIO m => DiffT Breadcrumbs m ()
breadcrumbsView = diffApply setBreadcrumbs
