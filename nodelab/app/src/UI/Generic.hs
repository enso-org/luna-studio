{-# LANGUAGE Rank2Types #-}

module UI.Generic where

import           GHCJS.Marshal.Pure        (pToJSVal)
import           Luna.Studio.Prelude         hiding (children)
import           Luna.Studio.Data.Vector

import qualified Event.Mouse               as Mouse
import           Object.Widget             (DragState (..), IsDisplayObject, WidgetFile, WidgetId, children, objectId, widget,
                                            widgetPosition)
import           Reactive.Commands.Command (Command, performIO)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.UIRegistry as UIRegistry
import qualified UI.Registry               as UIR
import           UI.Widget                 (GenericWidget (..), UIWidget)


foreign import javascript safe "$1.mesh.position.x = $2; $1.mesh.position.y = $3; $1.widgetMoved()"
    setWidgetPosition'      :: JSVal -> Double -> Double -> IO ()

foreign import javascript safe "$1.widgetMoved()"
    widgetMoved :: JSVal -> IO ()

foreign import javascript safe "$1.setSize($2, $3)"
    setSize'                :: GenericWidget -> Double -> Double -> IO ()

foreign import javascript safe "require('Rendering').removeWidget($1)"
    removeWidget :: Int -> IO ()

setWidgetPosition :: UIWidget a => Vector2 Double -> a -> IO ()
setWidgetPosition (Vector2 x y) widget = setWidgetPosition' (pToJSVal widget) x y

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

updatePosition' :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
updatePosition' widgetId position = do
    performIO $ do
        w <- UIR.lookup widgetId :: IO GenericWidget
        setWidgetPosition position w
    recursiveWidgetMoved widgetId

setSize :: WidgetId -> Vector2 Double -> IO ()
setSize wid (Vector2 x y) = do
    w <- UIR.lookup wid :: IO GenericWidget
    setSize' w x y

defaultResize :: WidgetId -> Vector2 Double -> a -> Command UIRegistry.State ()
defaultResize wid size _ = performIO $ setSize wid size

startDrag :: Mouse.Event' -> WidgetId -> Command Global.State ()
startDrag (Mouse.Event _ pos button keymods (Just (Mouse.EventWidget widgetId mat scene))) _ =
    Global.uiRegistry . UIRegistry.dragState ?= DragState widgetId mat scene button keymods pos pos pos
startDrag _ _ = return ()

abortDrag :: Command Global.State ()
abortDrag = Global.uiRegistry . UIRegistry.dragState .= Nothing

whenChanged :: (Eq b, Monad m) => a -> a -> Getter a b -> m () -> m ()
whenChanged old new get = when ((old ^. get) /= (new ^. get))
