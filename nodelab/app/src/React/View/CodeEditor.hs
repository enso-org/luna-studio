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
                        [ "background" Aeson..= ("#1e1e1e"::String)
                        , "margin-top" Aeson..= ("0px"    ::String)
                        , "height"     Aeson..= ("807px"  ::String)
                        , "width"      Aeson..= ("41px"   ::String)
                        ]
                    ] $ do div_ [ "className" $= "ace_gutter-cell" ] $ elemString "1"
                div_
                    [ "className" $= "ace_gutter-active-line"
                    , "style"     @= Aeson.object [ "height" Aeson..= ("19px"::String) ]
                    ] mempty
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


--TODO[react] remove
-- resizeTextEditorToggle :: Vector2 Int -> Command State ()
-- resizeTextEditorToggle screenSize = do
--     toggleId <- use $ Global.uiElements . UIElements.textEditorToggle
--     inRegistry $ do
--         let width = Style.textEditorToggle ^. Button.size . x
--         UICmd.moveX  toggleId $ (fromIntegral $ screenSize ^. x) - width
--         UICmd.resize toggleId $ Vector2 width (fromIntegral $ screenSize ^. y)
--
-- relayoutTextEditor :: Vector2 Int -> Command Global.State Int
-- relayoutTextEditor screenSize = do
--     visible <- use $ Global.uiElements . UIElements.textEditorVisible
--     performIO $ UI.setVisible visible
--     let width = (floor $ (0.3 :: Double) * (fromIntegral $ screenSize ^. x))
--     performIO $ UI.setWidth width
--
--     return $ if visible then width else 0
--
-- initTextEditor :: Command State ()
-- initTextEditor = do
--     let toggle = Style.textEditorToggle
--     toggleId <- inRegistry $ UICmd.register sceneInterfaceId toggle $ addHandler (Button.ClickedHandler $ const $ toggleText) mempty
--
--     Global.uiElements . UIElements.textEditorToggle .= toggleId
