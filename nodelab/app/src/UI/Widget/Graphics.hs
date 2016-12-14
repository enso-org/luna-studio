module UI.Widget.Graphics where

import           Luna.Studio.Prelude      hiding (Item)
import           Luna.Studio.Data.Vector

import           GHCJS.Marshal.Pure     (PFromJSVal (..), PToJSVal (..))

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Graphics as Model

import           UI.Generic             (whenChanged)
import qualified UI.Generic             as UI
import qualified UI.Registry            as UI
import           UI.Widget              (UIWidget)
import qualified UI.Widget              as Widget

import           Data.Aeson             (toJSON)
import           GHCJS.Marshal          (toJSVal)


newtype Graphics = Graphics JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Graphics

foreign import javascript safe "new Graphics($1, $2, $3)" create'    :: Int  -> Double -> Double -> IO Graphics
foreign import javascript safe "$1.setItems($2)"          setItems'  :: Graphics -> JSVal    -> IO ()
foreign import javascript safe "$1.setLabels($2)"         setLabels' :: Graphics -> JSVal    -> IO ()

create :: WidgetId -> Model.Graphics -> IO Graphics
create oid model = do
    widget   <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setItems  model widget
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

setItems :: Model.Graphics -> Graphics -> IO ()
setItems model widget = do
    items' <- toJSVal $ toJSON $ model ^. Model.items
    setItems' widget items'

setLabels :: Model.Graphics -> Graphics -> IO ()
setLabels model widget = do
    labels' <- toJSVal $ toJSON $ model ^. Model.labels
    setLabels' widget labels'

instance UIDisplayObject Model.Graphics where
    createUI parentId wid model = do
        widget   <- create wid model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid widget
        Widget.add widget parent
        setLabels model widget

    updateUI wid old model = do
        widget <- UI.lookup wid :: IO Graphics

        whenChanged old model Model.items  $ setItems  model widget
        whenChanged old model Model.labels $ setLabels model widget

instance CompositeWidget Model.Graphics
instance ResizableWidget Model.Graphics where resizeWidget = UI.defaultResize
