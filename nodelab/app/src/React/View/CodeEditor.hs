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
