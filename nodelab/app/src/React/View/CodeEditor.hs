{-# LANGUAGE OverloadedStrings #-}
module React.View.CodeEditor where

import qualified Data.HashMap.Strict    as HashMap
import           Data.Text.Lazy       (unpack)
import           React.Flux
import qualified React.Flux             as React
import           Utils.PreludePlus

import qualified React.Store.CodeEditor as CodeEditor
import           React.View.Node        (node_)


name :: JSString
name = "code-editor"


codeEditor :: CodeEditor.Ref -> ReactView ()
codeEditor ref = React.defineControllerView name ref $ \store () -> do
    div_ $ do
        elemString $ "code editor:"
        div_ $ do
            elemString $ unpack $ store ^. CodeEditor.code


codeEditor_ :: CodeEditor.Ref -> ReactElementM ViewEventHandler ()
codeEditor_ ref = React.view (codeEditor ref) () mempty
