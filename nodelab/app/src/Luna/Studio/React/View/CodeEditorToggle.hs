{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.CodeEditorToggle where

import qualified Luna.Studio.Event.UI               as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.CodeEditor as CodeEditor
import           Luna.Studio.React.Model.App        (App)
import           Luna.Studio.React.Store            (Ref, dispatch)
import           React.Flux
import qualified React.Flux                         as React


name :: JSString
name = "code-editor-toggle"

codeEditorToggle :: ReactView (Ref App)
codeEditorToggle = React.defineView name $ \ref-> do
    button_
        [ "key"       $= name
        , "className" $= name
        , onClick $ \_ _ -> dispatch ref $ UI.CodeEditorEvent CodeEditor.ToggleCodeEditor
        ] $ elemString $ []

codeEditorToggle_ :: Ref App -> ReactElementM ViewEventHandler ()
codeEditorToggle_ ref = React.viewWithSKey codeEditorToggle name ref mempty
