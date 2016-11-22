{-# LANGUAGE OverloadedStrings #-}
module React.View.Node where

import           React.Flux
import qualified React.Flux        as React
import           Utils.PreludePlus

import qualified React.Store.Node  as Node
import           React.Stores      (Stores)
import qualified React.Stores      as Stores



name :: JSString
name = "node-editor"


node :: Node.Ref -> ReactView ()
node nodeRef = React.defineControllerView
    name nodeRef $ \nodeStore () -> do
        div_ $ do
            elemString $ "node test"


node_ :: Node.Ref -> ReactElementM ViewEventHandler ()
node_ nodeRef = React.view (node nodeRef) () mempty
