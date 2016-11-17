{-# LANGUAGE OverloadedStrings #-}
module React.View.NodeEditor where

import           React.Flux
import qualified React.Flux             as React
import           Utils.PreludePlus

import qualified React.Store.NodeEditor as NodeEditor
import           React.Stores           (Stores)
import qualified React.Stores           as Stores



name :: JSString
name = "node-editor"


nodeEditor :: Stores -> ReactView ()
nodeEditor stores = React.defineControllerView
    name (stores ^. Stores.nodeEditor) $ \nodeEditorStore () -> do
        elemString $ "node editor test"


nodeEditor_ :: Stores -> ReactElementM ViewEventHandler ()
nodeEditor_ stores = React.view (nodeEditor stores) () mempty
