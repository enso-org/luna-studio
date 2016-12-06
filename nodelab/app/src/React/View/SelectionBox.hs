{-# LANGUAGE OverloadedStrings #-}
module React.View.SelectionBox where

import qualified Data.HashMap.Strict      as HashMap
import           React.Flux
import qualified React.Flux               as React
import           Utils.PreludePlus

import           React.Store              (Ref, dt)
import           React.Store.SelectionBox (SelectionBox)
import qualified React.Store.SelectionBox as SelectionBox


name :: JSString
name = "selection-box"


selectionBox :: Ref SelectionBox -> ReactView ()
selectionBox ref = React.defineControllerView name ref $ \store () -> do
    div_ $
        elemString $ show $ store ^. dt

selectionBox_ :: Ref SelectionBox -> ReactElementM ViewEventHandler ()
selectionBox_ ref = React.view (selectionBox ref) () mempty
