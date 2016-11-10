{-# LANGUAGE OverloadedStrings #-}
module React.View.Nodelab where

import           React.Flux
import qualified React.Flux          as React
import           Utils.PreludePlus

import qualified React.Store.Nodelab as Nodelab


name :: JSString
name = "nodelab"

nodelabApp :: ReactStore Nodelab.Store -> ReactView Nodelab.Props
nodelabApp reactStore = React.defineControllerView name reactStore $ \(Nodelab.Store i) (Nodelab.Props j) ->
    div_ $ do
        nodeEditor_ i
        nodeEditor_ j


nodeEditor :: Int -> ReactView ()
nodeEditor i = React.defineView "node editor" $ \() ->
    elemString $ "node editor test " <> show i


nodeEditor_ :: Int -> ReactElementM ViewEventHandler ()
nodeEditor_ i = React.view (nodeEditor i) () mempty
