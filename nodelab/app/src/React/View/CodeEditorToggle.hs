{-# LANGUAGE OverloadedStrings #-}
module React.View.CodeEditorToggle where

import qualified Event.UI               as UI
import qualified React.Event.CodeEditor as CodeEditor
import           React.Flux
import qualified React.Flux             as React
import           React.Store            (Ref, dispatch)
import           React.Model.CodeEditor (CodeEditor)
import           Luna.Studio.Prelude



name :: JSString
name = "code-editor-toggle"


codeEditorToggle :: Ref CodeEditor -> ReactView ()
codeEditorToggle ref = React.defineView name $ \() -> do
    button_ [ onClick $ \_ _ -> dispatch ref $ UI.CodeEditorEvent CodeEditor.ToggleCodeEditor
            , "className" $= "code-editor-toggle"
            ] $ do
        elemString $ []


codeEditorToggle_ :: Ref CodeEditor -> ReactElementM ViewEventHandler ()
codeEditorToggle_ ref = React.view (codeEditorToggle ref) () mempty
