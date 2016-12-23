module UI.Widget.Slider.Continuous where

import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude

import           Data.JSString.Text              (lazyTextToJSString)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Slider.Continuous as Model

import           UI.Generic                      (whenChanged)
import qualified UI.Generic                      as UI
import qualified UI.Registry                     as UI
import qualified UI.Widget                       as Widget
import           UI.Widget.Slider                (Slider, create', setFocus', setLabel', setValue')

createSlider :: WidgetId -> Model.ContinuousSlider -> IO Slider
createSlider oid model = do
    slider      <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setLabel       model slider
    setValue       model slider
    -- TODO[react]: Does not make sense anymore
    -- UI.setWidgetPosition (model ^. widgetPosition) slider
    return slider

setLabel :: Model.ContinuousSlider -> Slider -> IO ()
setLabel model slider = setLabel' slider $ lazyTextToJSString $ model ^. Model.label

setFocus :: Bool -> Slider -> IO ()
setFocus = flip setFocus'

setValue :: Model.ContinuousSlider -> Slider -> IO ()
setValue model slider = setValue' slider $ model ^. Model.boundedNormValue

instance UIDisplayObject Model.ContinuousSlider where
    createUI parentId wid model = do
        slider   <- createSlider wid model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid slider
        Widget.add slider parent

    updateUI wid old model = do
        slider <- UI.lookup wid :: IO Slider
        whenChanged old model Model.label $ setLabel model slider
        whenChanged old model Model.value $ setValue model slider
