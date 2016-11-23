{-# LANGUAGE OverloadedStrings #-}
module React.View.App where

import           React.Flux
import qualified React.Flux            as React
import           Utils.PreludePlus

import qualified React.Store.App       as App
import           React.View.NodeEditor (nodeEditor_)


name :: JSString
name = "nodelab"

app :: App.Ref -> ReactView ()
app ref = React.defineControllerView
    name ref $ \store () ->
    div_ $ do
        nodeEditor_ (store ^. App.nodeEditor)
