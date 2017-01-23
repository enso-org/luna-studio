{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.CodeEditor where

import qualified Data.Aeson                         as Aeson
import           Data.Text                          (unpack)
import           Luna.Studio.Prelude
import           React.Flux
import qualified React.Flux                         as React
import           Luna.Studio.React.Model.CodeEditor (CodeEditor)
import qualified Luna.Studio.React.Model.CodeEditor as CodeEditor


name :: JSString
name = "code-editor"

codeEditor :: ReactView CodeEditor
codeEditor = React.defineView name $ \model -> do
    let isVisible = model ^. CodeEditor.visible
        showFlag  = if isVisible then " code-editor--expanded" else " code-editor--collapsed"
        classes   = name <> showFlag
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
                        forM_ (zip [1..] $ lines $ unpack $ model ^. CodeEditor.code) $ \(i, line) ->
                            div_
                                [ "key"       $= jsShow i
                                , "className" $= "ace_active-line"
                                , "style"     @= Aeson.object
                                    [ "padding" Aeson..= ("0 4px" ::String) ]
                                ] $ do elemString line

codeEditor_ :: CodeEditor -> ReactElementM ViewEventHandler ()
codeEditor_ model = React.viewWithSKey codeEditor name model mempty


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
