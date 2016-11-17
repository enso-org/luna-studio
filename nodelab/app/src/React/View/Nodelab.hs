{-# LANGUAGE OverloadedStrings #-}
module React.View.Nodelab where

import           React.Flux
import qualified React.Flux          as React
import           Utils.PreludePlus

import qualified React.Store.Nodelab as Nodelab


name :: JSString
name = "nodelab"

nodelabApp :: ReactStore Nodelab.Store -> ReactStore Nodelab.Store -> ReactStore Nodelab.Store -> ReactView Nodelab.Props
nodelabApp store1 store2 reactStore = React.defineControllerView name reactStore $ \(Nodelab.Store i) (Nodelab.Props j) ->
    div_ $ do
        nodeEditor_ store1 i
        nodeEditor_ store2 j


nodeEditor :: ReactStore Nodelab.Store -> Int -> ReactView ()
nodeEditor s i = React.defineControllerView "node editor" s $ \(Nodelab.Store si) () ->
    elemString $ "node editor test " <> show si


nodeEditor_ :: ReactStore Nodelab.Store -> Int -> ReactElementM ViewEventHandler ()
nodeEditor_ s i = React.view (nodeEditor s i) () mempty
