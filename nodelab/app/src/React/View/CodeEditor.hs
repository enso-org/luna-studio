{-# LANGUAGE OverloadedStrings #-}
module React.View.CodeEditor where

import           Data.Text.Lazy         (unpack)
import           React.Flux
import qualified React.Flux             as React
import           Utils.PreludePlus

import           React.Store            (Ref, dt)
import           React.Store.CodeEditor (CodeEditor)
import qualified React.Store.CodeEditor as CodeEditor


name :: JSString
name = "code-editor"


codeEditor :: Ref CodeEditor -> ReactView ()
codeEditor ref = React.defineControllerView name ref $ \store () -> do
    div_ $ do
        elemString $ "code editor:"
        div_ $ do
            elemString $ unpack $ store ^. dt . CodeEditor.code


codeEditor_ :: Ref CodeEditor -> ReactElementM ViewEventHandler ()
codeEditor_ ref = React.view (codeEditor ref) () mempty
