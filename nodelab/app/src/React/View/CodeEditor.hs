{-# LANGUAGE OverloadedStrings #-}
module React.View.CodeEditor where

import qualified Data.Aeson             as Aeson
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
    div_
        [ "id"    $= "editorContainer"
        , "style" @= Aeson.object
            [ "width"      Aeson..= ("340px"::String)
            , "background" Aeson..= ("#222" ::String)
            ]
        ] $ do
        div_
            [ "id"        $= "editor"
            , "className" $= "ace_editor ace-twilight ace_dark"
            , "style"     @= Aeson.object [ "color" Aeson..= ("#eee"::String) ]
            ] $ do
            div_
                [ "className" $= "ace_gutter" ] $ do
                div_
                    [ "className" $= "ace_layer ace_gutter-layer ace_folding-enabled"
                    , "style"     @= Aeson.object
                        [ "background" Aeson..= ("#1e1e1e" ::String)
                        , "margin-top" Aeson..= ("0px"     ::String)
                        , "height"     Aeson..= ("807px"   ::String)
                        , "width"      Aeson..= ("41px"    ::String)
                        ]
                    ] $ do div_ [ "className" $= "ace_gutter-cell" ] $ elemString $ "1"
                div_
                    [ "className" $= "ace_gutter-active-line"
                    , "style"     @= Aeson.object [ "height" Aeson..= ("19px"::String) ]
                    ] $ mempty
            div_
                [ "className" $= "ace_scroller"
                , "style"     @= Aeson.object
                    [ "left"   Aeson..= ("41px"::String)
                    , "right"  Aeson..= ("0"   ::String)
                    , "bottom" Aeson..= ("0"   ::String)
                    ]
                ] $ do
                div_
                    [ "className" $= "ace_content"
                    , "style"     @= Aeson.object
                        [ "margin-top"  Aeson..= ("0"    ::String)
                        , "width"       Aeson..= ("276px"::String)
                        , "height"      Aeson..= ("663px"::String)
                        , "margin-left" Aeson..= ("0"    ::String)
                        ]
                    ] $ do
                    div_
                        [ "className" $= "ace_layer ace_text-layer" ]
                        $ do
                        div_
                            [ "className" $= "ace_active-line"
                            , "style"     @= Aeson.object [ "padding" Aeson..= ("0 4px" ::String) ]
                            ] $ do elemString $ unpack $ store ^. dt . CodeEditor.code


codeEditor_ :: Ref CodeEditor -> ReactElementM ViewEventHandler ()
codeEditor_ ref = React.view (codeEditor ref) () mempty
