{-# LANGUAGE OverloadedStrings #-}
module React.View.NodeEditor where

import qualified Data.HashMap.Strict    as HashMap
import           React.Flux
import qualified React.Flux             as React
import           Utils.PreludePlus

import qualified React.Store.NodeEditor as NodeEditor
import           React.View.Node        (node_)


name :: JSString
name = "node-editor"


nodeEditor :: NodeEditor.Ref -> ReactView ()
nodeEditor ref = React.defineControllerView name ref $ \store () -> do
    div_ $ do
        elemString $ "node editor:"
        forM_ (store ^. NodeEditor.nodes . to HashMap.elems) $ \nodeRef -> do
            node_ nodeRef


nodeEditor_ :: NodeEditor.Ref -> ReactElementM ViewEventHandler ()
nodeEditor_ ref = React.view (nodeEditor ref) () mempty
