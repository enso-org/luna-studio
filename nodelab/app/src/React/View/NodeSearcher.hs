{-# LANGUAGE OverloadedStrings #-}
module React.View.NodeSearcher where

import           React.Flux
import qualified React.Flux             as React
import           Utils.PreludePlus

import qualified Event.UI               as UI
import           React.Store            (Ref, dt)
import qualified React.Store            as Store
import           React.Store.NodeSearcher (NodeSearcher)
import qualified React.Store.NodeSearcher as NodeSearcher


name :: JSString
name = "node-searcher"


nodeSearcher :: Ref NodeSearcher -> ReactView ()
nodeSearcher nsRef = React.defineControllerView
    name nsRef $ \nsStore () -> do
        div_ $ elemString "NodeSearcher"


nodeSearcher_ :: Ref NodeSearcher -> ReactElementM ViewEventHandler ()
nodeSearcher_ nsRef = React.view (nodeSearcher nsRef) () mempty
