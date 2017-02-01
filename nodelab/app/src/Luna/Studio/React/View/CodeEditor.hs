{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.CodeEditor where

import qualified Data.Aeson                         as Aeson
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.CodeEditor (CodeEditor)
import qualified Luna.Studio.React.Model.CodeEditor as CodeEditor
import           React.Flux                         as React


name :: JSString
name = "code-editor"

codeEditor :: ReactView CodeEditor
codeEditor = React.defineView name $ \model -> do
    let isVisible = model ^. CodeEditor.visible
        showFlag  = if isVisible then " luna-code-editor--expanded" else " luna-code-editor--collapsed"
        classes   = "luna-" <> name <> " luna-noselect" <> showFlag
    div_
        [ "key"       $= name
        , "className" $= classes
        ] $ do
        div_
            [ "key"       $= "editor"
            , "id"        $= "editor"
            , "className" $= "ace_editor ace-twilight ace_dark"
            , "style"     @= Aeson.object
                [ "color" Aeson..= ("#eee"::String) ]
            ] $ do
            div_
                [ "key"       $= "ace_gutter"
                , "className" $= "ace_gutter"
                ] $ do
                div_
                    [ "key"       $= "ace_layer"
                    , "className" $= "ace_layer ace_gutter-layer ace_folding-enabled"
                    , "style"     @= Aeson.object
                        [ "background" Aeson..= ("#1e1e1e"::String)
                        , "marginTop"  Aeson..= ("0px"    ::String)
                        , "height"     Aeson..= ("807px"  ::String)
                        , "width"      Aeson..= ("41px"   ::String)
                        ]
                    ] $ do
                    div_
                        [ "key"       $= "cell"
                        , "className" $= "ace_gutter-cell"
                        ] $ elemString "1"
                div_
                    [ "key"       $= "active-line"
                    , "className" $= "ace_gutter-active-line"
                    , "style"     @= Aeson.object [ "height" Aeson..= ("19px"::String) ]
                    ] mempty
            div_
                [ "key" $= "scroller"
                , "className" $= "ace_scroller"
                , "style"     @= Aeson.object
                    [ "left"   Aeson..= ("41px"::String)
                    , "right"  Aeson..= ("0"   ::String)
                    , "bottom" Aeson..= ("0"   ::String)
                    ]
                ] $ do
                div_
                    [ "key"       $= "content"
                    , "className" $= "ace_content"
                    , "style"     @= Aeson.object
                        [ "marginTop"   Aeson..= ("0"    ::String)
                        , "width"       Aeson..= ("276px"::String)
                        , "height"      Aeson..= ("663px"::String)
                        , "marginLeft"  Aeson..= ("0"    ::String)
                        ]
                    ] $ do
                    div_
                        [ "key"       $= "content"
                        , "className" $= "ace_layer ace_text-layer" ]
                        $ do
                        forM_ (zip [1..] $ lines $ convert $ model ^. CodeEditor.code) $ \(i, line) ->
                            div_
                                [ "key"       $= jsShow i
                                , "className" $= "ace_active-line"
                                , "style"     @= Aeson.object
                                    [ "padding" Aeson..= ("0 4px" ::String) ]
                                ] $ do elemString line

codeEditor_ :: CodeEditor -> ReactElementM ViewEventHandler ()
codeEditor_ model = React.viewWithSKey codeEditor name model mempty
