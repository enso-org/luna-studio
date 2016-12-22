module UI.Widget.Plots.Image where

import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude

import           Data.JSString.Text        (lazyTextToJSString)
import           GHCJS.Marshal.Pure        (PFromJSVal (..), PToJSVal (..))

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Plots.Image as Model

import qualified UI.Generic                as UI
import qualified UI.Registry               as UI
import           UI.Widget                 (UIWidget)
import qualified UI.Widget                 as Widget

newtype Image = Image JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Image

foreign import javascript safe "new PlotImage($1, $2, $3)" create'  :: Int   -> Double -> Double -> IO Image
foreign import javascript safe "$1.setData($2)"        setData' :: Image -> JSString -> IO ()


create :: WidgetId -> Model.Image -> IO Image
create oid model = do
    plot <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setData' plot $ lazyTextToJSString $ model ^. Model.image
    -- TODO[react]: Does not make sense anymore
    -- UI.setWidgetPosition (model ^. widgetPosition) plot
    return plot

instance UIDisplayObject Model.Image where
    createUI parentId wid model = do
        plot   <- create wid model
        parent <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid plot
        Widget.add plot parent

    updateUI wid _ model = do
        plot <- UI.lookup wid :: IO Image
        setData' plot $ lazyTextToJSString $ model ^. Model.image

instance CompositeWidget Model.Image
instance ResizableWidget Model.Image
