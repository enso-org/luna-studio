{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.View.Breadcrumbs where

import           Common.Prelude

import qualified Control.Lens.Aeson                 as Lens
import qualified NodeEditor.React.Model.Breadcrumbs as B
import qualified NodeEditor.React.Model.App         as App

import Common.Data.JSON                   (toJSONVal)
import Data.Aeson                         (ToJSON (toEncoding, toJSON))
import Data.Convert                       (Convertible (convert))
import NodeEditor.React.Model.App         (App)
import NodeEditor.View.Diff               (DiffT, diffApply)


data BreadcrumbsView = BreadcrumbsView
    { _moduleName :: Maybe String
    , _items      :: [String]
    } deriving (Eq, Generic, Show)

makeLenses ''BreadcrumbsView

instance ToJSON BreadcrumbsView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible App BreadcrumbsView where
    convert a = BreadcrumbsView
        {- modeuleName -} (a ^. App.moduleName)
        {- items       -} (a ^. App.breadcrumbs . B.items . to names)
        where names = map $ convert . view B.name

foreign import javascript safe "atomCallback.getNodeEditorView().setBreadcrumbs($1)"
    setBreadcrumbs__ :: JSVal -> IO ()

setBreadcrumbs :: MonadIO m => BreadcrumbsView -> m ()
setBreadcrumbs = liftIO . setBreadcrumbs__ <=< toJSONVal

breadcrumbsView :: MonadIO m => DiffT BreadcrumbsView m ()
breadcrumbsView = diffApply setBreadcrumbs
