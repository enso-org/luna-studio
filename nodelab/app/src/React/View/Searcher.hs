{-# LANGUAGE OverloadedStrings #-}
module React.View.Searcher where

import           React.Flux
import qualified React.Flux           as React
import           Utils.PreludePlus

import qualified Event.UI             as UI
import           React.Store          (Ref, dt)
import qualified React.Store          as Store
import           React.Store.Searcher (Searcher)
import qualified React.Store.Searcher as Searcher


name :: JSString
name = "node-searcher"


searcher :: Ref Searcher -> ReactView ()
searcher ref = React.defineControllerView
    name ref $ \nsStore () -> do
        div_ $ elemString "Searcher"


searcher_ :: Ref Searcher -> ReactElementM ViewEventHandler ()
searcher_ ref = React.view (searcher ref) () mempty
