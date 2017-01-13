{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.App where

import           Luna.Studio.Prelude hiding (on)
import           React.Flux
import qualified React.Flux                              as React

import qualified Event.UI                                as UI
import qualified Luna.Studio.React.Event.App             as App
import           Luna.Studio.React.Model.App             (App)
import qualified Luna.Studio.React.Model.App             as App
import           Luna.Studio.React.Store                 (Ref, dispatch, dt)
import           Luna.Studio.React.View.Breadcrumbs      (breadcrumbs_)
import           Luna.Studio.React.View.CodeEditor       (codeEditor_)
import           Luna.Studio.React.View.CodeEditorToggle (codeEditorToggle_)
import           Luna.Studio.React.View.NodeEditor       (nodeEditor_)
import           Luna.Studio.React.View.Searcher         (searcher_)


name :: JSString
name = "app"

app :: Ref App -> ReactView ()
app ref = React.defineControllerView
    name ref $ \store () -> do
        let s = store ^. dt
        div_ [ onKeyDown     $ \e k -> preventDefault e : dispatch ref (UI.AppEvent $ App.KeyDown k)
             , onContextMenu $ \e _ -> [preventDefault e]
             , onMouseDown   $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseDown m
             , onMouseUp     $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseUp   m
             , onMouseMove   $ \_ m -> dispatch ref $ UI.AppEvent $ App.MouseMove m
             , "key"       $= "app"
             , "id"        $= "focus-root"
             , "tabIndex"  $= "-1"
             , "className" $= "noselect"
             ] $ do
                 div_ [ "className" $= "main"
                      , "key"       $= "main"] $ do
                     div_ [ "className" $= "graph-editor"
                          , "key"       $= "graph-editor" ] $ do
                        nodeEditor_ ref (s ^. App.nodeEditor)
                        breadcrumbs_ (s ^. App.breadcrumbs)
                        codeEditorToggle_ ref
                        searcher_ ref (s ^. App.searcher)
                     codeEditor_ (s ^. App.codeEditor)


foreign import javascript safe "document.getElementById('focus-root').focus()" focus :: IO ()
