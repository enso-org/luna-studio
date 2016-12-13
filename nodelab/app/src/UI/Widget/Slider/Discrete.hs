module UI.Widget.Slider.Discrete where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import           Data.JSString.Text            (lazyTextToJSString)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Slider.Discrete as Model

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import qualified UI.Widget                     as Widget
import           UI.Widget.Slider              (Slider, create', limitTicks, setFocus', setLabel', setTicks', setValue')

createSlider :: WidgetId -> Model.DiscreteSlider -> IO Slider
createSlider oid model = do
    slider      <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model slider
    setValue       model slider
    setTicks       model slider
    UI.setWidgetPosition (model ^. widgetPosition) slider
    return slider

setLabel :: Model.DiscreteSlider -> Slider -> IO ()
setLabel model slider = setLabel' slider $ lazyTextToJSString $ model ^. Model.label

setFocus :: Bool -> Slider -> IO ()
setFocus = flip setFocus'

setValue :: Model.DiscreteSlider -> Slider -> IO ()
setValue model slider = setValue' slider $ model ^. Model.boundedNormValue

setTicks :: Model.DiscreteSlider -> Slider -> IO ()
setTicks model slider = setTicks' slider True offset span where
    (offset, span) = limitTicks (model ^. Model.minValue) (model ^. Model.maxValue) (model ^. Model.size . x)

instance UIDisplayObject Model.DiscreteSlider where
    createUI parentId wid model = do
        slider   <- createSlider wid model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid slider
        Widget.add slider parent

    updateUI wid old model = do
        slider <- UI.lookup wid :: IO Slider
        whenChanged old model Model.label $ setLabel model slider
        whenChanged old model Model.value $ setValue model slider
        whenChanged old model Model.range $ setTicks model slider
