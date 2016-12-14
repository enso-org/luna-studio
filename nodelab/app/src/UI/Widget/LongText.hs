{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module UI.Widget.LongText where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import qualified Data.JSString          as JSString
import           Data.JSString.Text     (lazyTextToJSString)
import           GHCJS.Marshal.Pure     (PFromJSVal (..), PToJSVal (..))

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.LongText as Model

import           UI.Generic             (whenChanged)
import qualified UI.Generic             as UI
import qualified UI.Registry            as UI
import           UI.Widget              (UIWidget)
import qualified UI.Widget              as Widget


newtype LongText = LongText JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget LongText

foreign import javascript safe "new LongText($1, $2, $3)" create'      :: Int     -> Double -> Double -> IO LongText
foreign import javascript safe "$1.setAlignment($2)"     setAlignment' :: LongText -> JSString         -> IO ()
foreign import javascript safe "$1.setText($2)"          setText'      :: LongText -> JSString         -> IO ()
foreign import javascript safe "$1.setMonospace($2)"     setMonospace' :: LongText -> Bool         -> IO ()
foreign import javascript safe "$1.relayout()"           realayout'    :: LongText          -> IO ()

create :: WidgetId -> Model.LongText -> IO LongText
create oid model = do
    textBox      <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setAlignment   model textBox
    setText        model textBox
    setMonospace   model textBox
    UI.setWidgetPosition (model ^. widgetPosition) textBox
    return textBox

setText :: Model.LongText -> LongText -> IO ()
setText model textBox = setText' textBox $ lazyTextToJSString $ model ^. Model.value

setMonospace :: Model.LongText -> LongText -> IO ()
setMonospace model textBox = setMonospace' textBox $ model ^. Model.tpe == Model.Code

setAlignment :: Model.LongText -> LongText -> IO ()
setAlignment model textBox = setAlignment' textBox $ JSString.pack $ show $ model ^. Model.alignment

instance UIDisplayObject Model.LongText where
    createUI parentId wid model = do
        textBox   <- create wid model
        parent    <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid textBox
        Widget.add textBox parent
        realayout' textBox

    updateUI wid old model = do
        textBox <- UI.lookup wid :: IO LongText

        whenChanged old model Model.alignment $ setAlignment   model textBox
        whenChanged old model Model.value     $ setText        model textBox
        whenChanged old model Model.tpe       $ setMonospace   model textBox

instance CompositeWidget Model.LongText
instance ResizableWidget Model.LongText where
    resizeWidget = UI.defaultResize
