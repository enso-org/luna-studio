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
    name ref $ \store () -> do
        let s = store ^. dt
        when (s ^. Searcher.visible) $ do
            div_ [ "className" $= "searcher"
                 , "style"     @= Aeson.object [ "top"  Aeson..= ("50%"::String)
                                               , "left" Aeson..= ("50%"::String)
                                               ]
                 ] $ do
                     input_ mempty


searcher_ :: Ref Searcher -> ReactElementM ViewEventHandler ()
searcher_ ref = React.view (searcher ref) () mempty
