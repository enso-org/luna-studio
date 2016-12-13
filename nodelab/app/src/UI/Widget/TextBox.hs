{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module UI.Widget.TextBox where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import qualified Data.JSString         as JSString
import           Data.JSString.Text    (lazyTextToJSString)
import           GHCJS.Marshal.Pure    (PFromJSVal (..), PToJSVal (..))

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.TextBox as Model

import           UI.Generic            (whenChanged)
import qualified UI.Generic            as UI
import qualified UI.Registry           as UI
import           UI.Widget             (UIWidget)
import qualified UI.Widget             as Widget


newtype TextBox = TextBox JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget TextBox

foreign import javascript safe "new TextBox($1, $2, $3)" create'        :: Int     -> Double -> Double -> IO TextBox
foreign import javascript safe "$1.setAlignment($2)"     setAlignment'  :: TextBox -> JSString         -> IO ()
foreign import javascript safe "$1.setValueLabel($2)"    setValueLabel' :: TextBox -> JSString         -> IO ()
foreign import javascript safe "$1.startEditing($2)"     startEditing'  :: TextBox -> JSString         -> IO ()
foreign import javascript safe "$1.doneEditing()"        doneEditing'   :: TextBox                     -> IO ()

create :: WidgetId -> Model.TextBox -> IO TextBox
create oid model = do
    textBox      <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setAlignment   model textBox
    UI.setWidgetPosition (model ^. widgetPosition) textBox
    return textBox

setValueLabel :: Model.TextBox -> TextBox -> IO ()
setValueLabel model textBox = setValueLabel' textBox $ lazyTextToJSString $ model ^. Model.value

setAlignment :: Model.TextBox -> TextBox -> IO ()
setAlignment model textBox = setAlignment' textBox $ JSString.pack $ show $ model ^. Model.alignment

instance UIDisplayObject Model.TextBox where
    createUI parentId wid model = do
        textBox   <- create wid model
        parent    <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid textBox
        Widget.add textBox parent
        setValueLabel  model textBox

    updateUI wid old model = do
        textBox <- UI.lookup wid :: IO TextBox

        whenChanged old model Model.alignment $ setAlignment   model textBox
        whenChanged old model Model.value     $ setValueLabel  model textBox
        whenChanged old model Model.isEditing $ do
            if old ^. Model.isEditing then doneEditing'  textBox
                                      else startEditing' textBox $ lazyTextToJSString $ model ^. Model.value

instance CompositeWidget Model.TextBox
instance ResizableWidget Model.TextBox where
    resizeWidget = UI.defaultResize
