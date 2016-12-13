module UI.Widget.Button where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import qualified Data.JSString        as JSString
import           Data.JSString.Text   (lazyTextToJSString)
import           GHCJS.Marshal.Pure   (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Nullable       (Nullable, maybeToNullable)

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Button as Model

import           Style.Types          (Color (..))
import           UI.Generic           (whenChanged)
import qualified UI.Generic           as UI
import qualified UI.Registry          as UI
import           UI.Widget            (UIWidget)
import qualified UI.Widget            as Widget

newtype Button = Button JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Button

foreign import javascript safe "new Button($1, $2, $3)"        create'       :: Int    -> Double -> Double  -> IO Button
foreign import javascript safe "$1.setLabel($2)"               setLabel'     :: Button -> JSString          -> IO ()
foreign import javascript safe "$1.setIcon($2)"                setIcon'      :: Button -> Nullable JSString -> IO ()
foreign import javascript safe "$1.setEnabled($2)"             setEnabled'   :: Button -> Bool              -> IO ()
foreign import javascript safe "$1.setRounded($2)"             setRounded'   :: Button -> Bool              -> IO ()
foreign import javascript safe "$1.setBgColor($2, $3, $4, $5)" setBgColor'   :: Button -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript safe "$1.setAlignment($2)"           setAlignment' :: Button -> JSString -> IO ()

create :: WidgetId -> Model.Button -> IO Button
create oid model = do
    widget      <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setEnabled     model widget
    setIcon        model widget
    setRounded     model widget
    setBgColor     model widget
    setAlignment   model widget
    UI.setWidgetPosition (model ^. widgetPosition) widget
    return widget

setLabel :: Model.Button -> Button -> IO ()
setLabel model widget = setLabel' widget $ lazyTextToJSString $ model ^. Model.label

setRounded :: Model.Button -> Button -> IO ()
setRounded model widget = setRounded' widget $ model ^. Model.style . Model.rounded

setBgColor :: Model.Button -> Button -> IO ()
setBgColor model widget = setBgColor' widget r g b a where (Color r g b a) =  model ^. Model.style . Model.background

setIcon :: Model.Button -> Button -> IO ()
setIcon model widget = setIcon' widget $ maybeToNullable (lazyTextToJSString <$> model ^. Model.icon)

setEnabled :: Model.Button -> Button -> IO ()
setEnabled model widget = setEnabled' widget $ model ^. Model.enabled

setAlignment :: Model.Button -> Button -> IO ()
setAlignment model label = setAlignment' label $ JSString.pack $ show $ model ^. Model.style . Model.alignment

instance UIDisplayObject Model.Button where
    createUI parentId wid model = do
        widget   <- create wid model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid widget
        Widget.add widget parent
        setLabel       model widget

    updateUI wid old model = do
        widget <- UI.lookup wid :: IO Button
        whenChanged old model Model.label   $ setLabel   model widget
        whenChanged old model Model.icon    $ setIcon    model widget
        whenChanged old model Model.enabled $ setEnabled model widget
        whenChanged old model Model.style   $ do
            setRounded   model widget
            setBgColor   model widget
            setAlignment model widget

instance CompositeWidget Model.Button

instance ResizableWidget Model.Button where resizeWidget = UI.defaultResize
