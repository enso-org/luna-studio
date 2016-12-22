module UI.Widget.Label where

import           Luna.Studio.Prelude

import qualified Data.JSString           as JSString
import           Data.JSString.Text      (lazyTextToJSString)
import           GHCJS.Marshal.Pure      (PFromJSVal (..), PToJSVal (..))
import           Luna.Studio.Data.Vector

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Label     as Model

import qualified UI.Generic              as UI
import qualified UI.Registry             as UI
import           UI.Widget               (UIWidget)
import qualified UI.Widget               as Widget

newtype Label = Label JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Label

foreign import javascript safe "new Label($1, $2, $3)" create'       :: Int -> Double -> Double -> IO Label
foreign import javascript safe "$1.setLabel($2)"       setLabel'     :: Label -> JSString -> IO ()
foreign import javascript safe "$1.setAlignment($2)"   setAlignment' :: Label -> JSString -> IO ()
foreign import javascript safe "$1.setMonospace($2)"   setMonospace' :: Label -> Bool     -> IO ()

create :: WidgetId -> Model.Label -> IO Label
create oid model = do
    widget      <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setAlignment model widget
    setMonospace model widget
    -- TODO[react]: Does not make sense anymore
    -- UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

setLabel :: Model.Label -> Label -> IO ()
setLabel model widget = setLabel' widget $ lazyTextToJSString $ model ^. Model.label

setAlignment :: Model.Label -> Label -> IO ()
setAlignment model label = setAlignment' label $ JSString.pack $ show $ model ^. Model.alignment

setMonospace :: Model.Label -> Label -> IO ()
setMonospace model label = setMonospace' label isMono where
    isMono = case model ^. Model.fontStyle of
        Model.SansSerif -> False
        Model.Monospace -> True

instance UIDisplayObject Model.Label where
    createUI parentId wid model = do
        widget   <- create wid model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid widget
        Widget.add widget parent
        setLabel       model widget

    updateUI wid _old model = do
        widget <- UI.lookup wid :: IO Label
        setLabel     model widget
        setAlignment model widget
        setMonospace model widget

instance CompositeWidget Model.Label
instance ResizableWidget Model.Label where resizeWidget = UI.defaultResize
