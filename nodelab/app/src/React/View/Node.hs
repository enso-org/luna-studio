{-# LANGUAGE OverloadedStrings #-}
module React.View.Node where

import           React.Flux
import qualified React.Flux        as React
import           Utils.PreludePlus

import           React.Store       (Ref, dt)
import qualified React.Store       as Store
import           React.Store.Node  (Node)
import qualified React.Store.Node  as Node



name :: JSString
name = "node-editor"


node :: Ref Node -> ReactView ()
node nodeRef = React.defineControllerView
    name nodeRef $ \nodeStore () -> do
        div_ [onClick $ \_ _ -> Store.dispatch nodeRef $ Store.NodeEvent Node.OnClick] $ do
            elemString $ "node: " <> show (nodeStore ^. dt . Node.name)


node_ :: Ref Node -> ReactElementM ViewEventHandler ()
node_ nodeRef = React.view (node nodeRef) () mempty
