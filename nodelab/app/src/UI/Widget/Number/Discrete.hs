module UI.Widget.Number.Discrete where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import           Data.JSString.Text            (lazyTextToJSString)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Number.Discrete as Model

import           UI.Generic                    (whenChanged)
import qualified UI.Generic                    as UI
import qualified UI.Registry                   as UI
import qualified UI.Widget                     as Widget
import           UI.Widget.Number              (Number, create', setLabel')

createNumber :: WidgetId -> Model.DiscreteNumber -> IO Number
createNumber oid model = do
    slider   <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    UI.setWidgetPosition (model ^. Model.position) slider
    return slider

setLabel :: Model.DiscreteNumber-> Number -> IO ()
setLabel model slider = setLabel' slider $ lazyTextToJSString $ model ^. Model.label

instance UIDisplayObject Model.DiscreteNumber where
    createUI parentId wid model = do
        slider   <- createNumber wid model
        parent <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid slider
        Widget.add slider parent
        setLabel    model slider

    updateUI wid old model = do
        slider <- UI.lookup wid :: IO Number
        whenChanged old model Model.label $ setLabel model slider
