{-# LANGUAGE OverloadedStrings #-}
module React.View.CodeEditorToggle where

import qualified Event.UI          as UI
import           React.Flux
import qualified React.Flux        as React
import           React.Store       (Ref, dispatch)
import           React.Store.App   (App)
import qualified React.Store.App   as App
import           Utils.PreludePlus



name :: JSString
name = "code-editor-toggle"


codeEditorToggle :: Ref App -> ReactView ()
codeEditorToggle ref = React.defineView name $ \() -> do
    button_ [ onClick       $ \_ _ -> dispatch ref $ UI.AppEvent App.ToggleCodeEditor] $ do
        elemString $ "toggle code editor"


codeEditorToggle_ :: Ref App -> ReactElementM ViewEventHandler ()
codeEditorToggle_ ref = React.view (codeEditorToggle ref) () mempty
