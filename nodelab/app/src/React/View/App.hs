{-# LANGUAGE OverloadedStrings #-}
module React.View.App where

import           React.Flux
import qualified React.Flux                  as React
import           Utils.PreludePlus

import qualified Event.UI                    as UI
import           React.Store                 (Ref, dispatch, dt)
import           React.Store.App             (App)
import qualified React.Store.App             as App
import qualified React.Store.NodeSearcher    as NS
import           React.View.Breadcrumbs      (breadcrumbs_)
import           React.View.CodeEditor       (codeEditor_)
import           React.View.CodeEditorToggle (codeEditorToggle_)
import           React.View.NodeEditor       (nodeEditor_)
import           React.View.NodeSearcher     (nodeSearcher_)



name :: JSString
name = "nodelab"

app :: Ref App -> ReactView ()
app ref = React.defineControllerView
    name ref $ \store () -> do
        let s = store ^. dt
        div_ [onKeyDown $ \_ _ -> dispatch (s ^. App.nodeSearcher) $ UI.NodeSearcherEvent $ NS.Display
             , "tabindex" $= "0"] $ do
            breadcrumbs_ (s ^. App.breadcrumbs)
            nodeEditor_ (s ^. App.nodeEditor)
            codeEditorToggle_ ref
            when (s ^. App.codeEditorVisible) $
                codeEditor_ (s ^. App.codeEditor)
            nodeSearcher_ (s ^. App.nodeSearcher)
