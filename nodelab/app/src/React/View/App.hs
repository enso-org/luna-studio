{-# LANGUAGE OverloadedStrings #-}
module React.View.App where

import           React.Flux
import qualified React.Flux                  as React
import           Utils.PreludePlus

import           React.Store                 (Ref, dt)
import           React.Store.App             (App)
import qualified React.Store.App             as App
import           React.View.Breadcrumbs      (breadcrumbs_)
import           React.View.CodeEditor       (codeEditor_)
import           React.View.CodeEditorToggle (codeEditorToggle_)
import           React.View.NodeEditor       (nodeEditor_)
import           React.View.NodeSearcher       (nodeSearcher_)



name :: JSString
name = "nodelab"

app :: Ref App -> ReactView ()
app ref = React.defineControllerView
    name ref $ \store () ->
        div_ $ do
            breadcrumbs_ (store ^. dt . App.breadcrumbs)
            nodeEditor_ (store ^. dt . App.nodeEditor)
            codeEditorToggle_ ref
            when (store ^. dt . App.codeEditorVisible) $
                codeEditor_ (store ^. dt . App.codeEditor)
            nodeSearcher_ (store ^. dt . App.nodeSearcher)
