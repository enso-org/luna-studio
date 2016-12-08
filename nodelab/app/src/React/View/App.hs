{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
module React.View.App where

import           React.Flux
import qualified React.Flux                  as React
import           Utils.PreludePlus

import qualified Event.UI                    as UI
import           React.Store                 (Ref, dispatch, dt)
import           React.Store.App             (App)
import qualified React.Store.App             as App
import           React.View.Breadcrumbs      (breadcrumbs_)
import           React.View.CodeEditor       (codeEditor_)
import           React.View.CodeEditorToggle (codeEditorToggle_)
import           React.View.NodeEditor       (nodeEditor_)
import           React.View.Searcher         (searcher_)



name :: JSString
name = "nodelab"

app :: Ref App -> ReactView ()
app ref = React.defineControllerView
    name ref $ \store () -> do
        let s = store ^. dt
        div_ [ onKeyDown   $ \e k -> preventDefault e : dispatch ref (UI.AppEvent $ App.KeyDown k)
             , onMouseDown $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseDown m
             , onMouseUp   $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseUp   m
             , onMouseMove $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseMove m
             , "id"       $= "focus-root"
             , "tabIndex" $= "-1"] $ do
            breadcrumbs_ (s ^. App.breadcrumbs)
            nodeEditor_ (s ^. App.nodeEditor)
            codeEditorToggle_ (s ^. App.codeEditor)
            codeEditor_ (s ^. App.codeEditor)
            searcher_ (s ^. App.searcher)

foreign import javascript safe "document.getElementById('focus-root').focus()" focus :: IO ()
