{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.App where

import           React.Flux
import qualified React.Flux                  as React
import           Luna.Studio.Prelude

import qualified Event.UI                    as UI
import           Luna.Studio.React.Store                 (Ref, dispatch, dt)
import           Luna.Studio.React.Model.App             (App)
import qualified Luna.Studio.React.Model.App             as App
import qualified Luna.Studio.React.Event.App             as App
import           Luna.Studio.React.View.Breadcrumbs      (breadcrumbs_)
import           Luna.Studio.React.View.CodeEditor       (codeEditor_)
import           Luna.Studio.React.View.CodeEditorToggle (codeEditorToggle_)
import           Luna.Studio.React.View.NodeEditor       (nodeEditor_)
import           Luna.Studio.React.View.Searcher         (searcher_)


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
             , "tabIndex" $= "-1"
             , "className" $= "noselect"
             ] $ do
                 div_ [ "className" $= "main" ] $ do
                     div_ [ "className" $= "graph-editor" ] $ do
                        breadcrumbs_ (s ^. App.breadcrumbs)
                        nodeEditor_ (s ^. App.nodeEditor)
                        codeEditorToggle_ (s ^. App.codeEditor)
                        searcher_ (s ^. App.searcher)
                     codeEditor_ (s ^. App.codeEditor)


foreign import javascript safe "document.getElementById('focus-root').focus()" focus :: IO ()
