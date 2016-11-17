{-# LANGUAGE OverloadedStrings #-}
module React.View.Nodelab where

import           React.Flux
import qualified React.Flux            as React
import           Utils.PreludePlus

import           React.Stores          (Stores)
import           React.View.NodeEditor (nodeEditor_)


name :: JSString
name = "nodelab"

nodelabApp :: Stores -> ReactView ()
nodelabApp stores = React.defineView
    name $ \() ->
    div_ $ do
        nodeEditor_ stores
