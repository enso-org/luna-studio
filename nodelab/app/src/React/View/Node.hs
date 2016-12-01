{-# LANGUAGE OverloadedStrings #-}
module React.View.Node where

import           React.Flux
import qualified React.Flux        as React
import           Utils.PreludePlus

import qualified Event.UI          as UI
import           React.Store       (Ref, dt)
import qualified React.Store       as Store
import           React.Store.Node  (Node)
import qualified React.Store.Node  as Node


name :: JSString
name = "node-editor"


node :: Ref Node -> ReactView ()
node nodeRef = React.defineControllerView
    name nodeRef $ \nodeStore () -> do
        let n = nodeStore ^. dt
        div_ [ onClick       $ \_ _ -> Store.dispatch nodeRef $ UI.NodeEvent Node.OnClick
             , onDoubleClick $ \_ _ -> Store.dispatch nodeRef $ UI.NodeEvent $ Node.Enter $ n ^. Node.nodeId
             ] $ do
            elemString $ "node: " <> show (n ^. Node.name)


node_ :: Ref Node -> ReactElementM ViewEventHandler ()
node_ nodeRef = React.view (node nodeRef) () mempty
