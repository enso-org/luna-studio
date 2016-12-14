module UI.Widget.Group where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import           GHCJS.Marshal.Pure           (PFromJSVal (..), PToJSVal (..))

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Group          as Model
import qualified Luna.Studio.Commands.UIRegistry as UICmd

import           Style.Types                  (Color (..), Padding (..))
import           UI.Generic                   (whenChanged)
import qualified UI.Generic                   as UI
import qualified UI.Registry                  as UI
import           UI.Widget                    (UIWidget)
import qualified UI.Widget                    as Widget

newtype Group = Group JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget Group

foreign import javascript safe "new Group($1, $2, $3)"         create'       :: Int   -> Double -> Double -> IO Group
foreign import javascript safe "$1.setVisible($2)"             setVisible'   :: Group -> Bool -> IO ()
foreign import javascript safe "$1.setBgVisible($2)"           setBgVisible' :: Group -> Bool -> IO ()
foreign import javascript safe "$1.setBorderRadius($2, $3, $4, $5)" setBorderRadius' :: Group -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript safe "$1.setPadding($2, $3, $4, $5)" setPadding'   :: Group -> Double -> Double -> Double -> Double -> IO ()
foreign import javascript safe "$1.setBgColor($2, $3, $4, $5)" setBgColor'   :: Group -> Double -> Double -> Double -> Double -> IO ()

setBgColor :: Group -> Model.Group -> IO ()
setBgColor group model = case model ^. Model.style . Model.background of
    Just (Color r g b a) -> do
        setBgColor' group r g b a
        setBgVisible' group True
    Nothing -> setBgVisible' group False

setPadding :: Group -> Model.Group -> IO ()
setPadding group model = do
    let (Padding top right bottom left) = model ^. Model.style . Model.padding
    setPadding' group top right bottom left

setBorderRadius :: Group -> Model.Group -> IO ()
setBorderRadius group model = do
    let (a, b, c, d) = model ^. Model.style . Model.borderRadius
    setBorderRadius' group a b c d

create :: WidgetId -> Model.Group -> IO Group
create oid model = do
    group      <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    setBgColor group model
    setPadding group model
    setBorderRadius group model
    setVisible' group $ model ^. Model.visible
    UI.setWidgetPosition (model ^. widgetPosition) group
    return group

instance UIDisplayObject Model.Group where
    createUI parentId wid model = do
        group    <- create wid model
        parent   <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid group
        Widget.add group parent

    updateUI wid old model = do
        group <- UI.lookup wid :: IO Group
        whenChanged old model Model.visible $ setVisible' group $ model ^. Model.visible
        whenChanged old model (Model.style . Model.background  ) $ setBgColor  group model
        whenChanged old model (Model.style . Model.borderRadius) $ setBorderRadius  group model
        whenChanged old model (Model.style . Model.padding     ) $ setPadding group model

instance CompositeWidget Model.Group where
    updateWidget wid old model = do
        let vis = model ^. Model.visible
        when (old ^. Model.visible /= vis) $ do
            parent <- UICmd.parent wid
            UICmd.triggerChildrenResized parent
