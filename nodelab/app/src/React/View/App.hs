{-# LANGUAGE OverloadedStrings #-}
module React.View.App where

import           React.Flux
import qualified React.Flux            as React
import           Utils.PreludePlus

import           React.Store           (Ref, dt)
import           React.Store.App       (App)
import qualified React.Store.App       as App
import           React.View.CodeEditor (codeEditor_)
import           React.View.NodeEditor (nodeEditor_)


name :: JSString
name = "nodelab"

app :: Ref App -> ReactView ()
app ref = React.defineControllerView
    name ref $ \store () ->
        div_ $ do
            div_ $ nodeEditor_ (store ^. dt . App.nodeEditor)
            div_ $ codeEditor_ (store ^. dt . App.codeEditor)
