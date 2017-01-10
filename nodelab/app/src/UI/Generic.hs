{-# LANGUAGE Rank2Types #-}

module UI.Generic where

import           GHCJS.Marshal.Pure           (pToJSVal)
import           Luna.Studio.Data.Vector      (Position, Size, x, y)
import           Luna.Studio.Prelude          hiding (children)

import qualified Event.Mouse                  as Mouse
import           Luna.Studio.Action.Command (Command, performIO)
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.UIRegistry as UIRegistry
import           Object.Widget                (DragState (..), IsDisplayObject, WidgetFile, WidgetId, children, objectId, widget,
                                               widgetPosition)
import qualified UI.Registry                  as UIR
import           UI.Widget                    (GenericWidget (..), UIWidget)


foreign import javascript safe "$1.mesh.position.x = $2; $1.mesh.position.y = $3; $1.widgetMoved()"
    setWidgetPosition'      :: JSVal -> Double -> Double -> IO ()

foreign import javascript safe "$1.widgetMoved()"
    widgetMoved :: JSVal -> IO ()

foreign import javascript safe "$1.setSize($2, $3)"
    setSize'                :: GenericWidget -> Double -> Double -> IO ()

foreign import javascript safe "require('Rendering').removeWidget($1)"
    removeWidget :: Int -> IO ()

setWidgetPosition :: UIWidget a => Position -> a -> IO ()
setWidgetPosition pos widget = setWidgetPosition' (pToJSVal widget) (pos ^. x) (pos ^. y)

updatePosition :: (IsDisplayObject b) => WidgetFile b -> Command UIRegistry.State ()
updatePosition file = do
    let position = file ^. widget . widgetPosition
        widgetId = file ^. objectId
    performIO $ do
        w <- UIR.lookup widgetId :: IO GenericWidget
        setWidgetPosition position w
    recursiveWidgetMoved widgetId

recursiveWidgetMoved :: WidgetId -> Command UIRegistry.State ()
recursiveWidgetMoved widgetId = do
    wf <- UIRegistry.lookupM widgetId
    withJust wf $ \wf ->
        forM_ (wf ^. children) $ \widgetId -> do
            performIO $ do
                w <- UIR.lookup widgetId :: IO GenericWidget
                widgetMoved (pToJSVal w)
            recursiveWidgetMoved widgetId

updatePosition' :: WidgetId -> Position -> Command UIRegistry.State ()
updatePosition' widgetId position = do
    performIO $ do
        w <- UIR.lookup widgetId :: IO GenericWidget
        setWidgetPosition position w
    recursiveWidgetMoved widgetId

setSize :: WidgetId -> Size -> IO ()
setSize wid size = do
    w <- UIR.lookup wid :: IO GenericWidget
    setSize' w (size ^. x) (size ^. y)

defaultResize :: WidgetId -> Size -> a -> Command UIRegistry.State ()
defaultResize wid size _ = performIO $ setSize wid size

startDrag :: Mouse.Event' -> WidgetId -> Command Global.State ()
startDrag (Mouse.Event _ pos button keymods (Just (Mouse.EventWidget widgetId mat scene))) _ =
    Global.uiRegistry . UIRegistry.dragState ?= DragState widgetId mat scene button keymods pos pos pos
startDrag _ _ = return ()

abortDrag :: Command Global.State ()
abortDrag = Global.uiRegistry . UIRegistry.dragState .= Nothing

whenChanged :: (Eq b, Monad m) => a -> a -> Getter a b -> m () -> m ()
whenChanged old new get = when ((old ^. get) /= (new ^. get))
