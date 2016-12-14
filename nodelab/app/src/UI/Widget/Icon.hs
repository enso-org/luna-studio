module UI.Widget.Icon where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import           Data.JSString.Text (lazyTextToJSString)
import           GHCJS.Marshal.Pure (PFromJSVal (..), PToJSVal (..))

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Icon as Model

import           UI.Generic         (whenChanged)
import qualified UI.Generic         as UI
import qualified UI.Registry        as UI
import           UI.Widget          (UIWidget)
import qualified UI.Widget          as Widget

newtype Icon = Icon JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Icon

foreign import javascript safe "new Icon($1, $2, $3)" create'  :: Int  -> Double -> Double -> IO Icon
foreign import javascript safe "$1.setIcon($2)"       setIcon' :: Icon -> JSString         -> IO ()

create :: WidgetId -> Model.Icon -> IO Icon
create oid model = do
    widget      <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setIcon model widget
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

setIcon :: Model.Icon -> Icon -> IO ()
setIcon model widget = setIcon' widget $ lazyTextToJSString $ model ^. Model.shader

instance UIDisplayObject Model.Icon where
    createUI parentId wid model = do
        widget   <- create wid model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid widget
        Widget.add widget parent

    updateUI wid old model = do
        widget <- UI.lookup wid :: IO Icon

        whenChanged old model Model.shader    $ setIcon model widget

instance CompositeWidget Model.Icon
instance ResizableWidget Model.Icon where resizeWidget = UI.defaultResize
