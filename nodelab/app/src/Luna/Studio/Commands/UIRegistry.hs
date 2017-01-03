{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Studio.Commands.UIRegistry
    ( LostFocus(..)
    , ChildrenResizedHandler(..)
    , children
    , get
    , get'
    , handler
    , lookup
    , maybeGet
    , move
    , moveBy
    , moveX
    , moveY
    , parent
    , removeWidget
    , resize
    , resize'
    , resizeNoCB
    , register
    , register_
    , registerIx
    , registerIx_
    , takeFocus
    , triggerChildrenResized
    , tryUpdate
    , update
    , update'
    , update_
    ) where

import           Data.HMap.Lazy               (HTMap, TypeKey (..))
import qualified Data.HMap.Lazy               as HMap
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude          hiding (children, lookup)

import qualified JS.Cursor                    as Cursor
import           Luna.Studio.Commands.Command (Command, performIO)
import qualified Luna.Studio.State.Global     as Global
import qualified Luna.Studio.State.UIRegistry as UIRegistry
import           Object.Widget                (CompositeWidget, WidgetId, createWidget, fromWidgetId, objectId, resizeWidget, updateWidget,
                                               widget, widgetPosition, widgetSize)
import qualified Object.Widget                as Widget
import qualified UI.Generic                   as UI

-- TODO: extract duplicated code in two functions beneath
register :: (CompositeWidget a, Widget.DisplayObjectClass a) => WidgetId -> a -> HTMap -> Command UIRegistry.State WidgetId
register parentId model handlers = do
    file <- UIRegistry.registerM parentId model handlers
    performIO $ Widget.createUI parentId (file ^. objectId) model
    createWidget (file ^. objectId) model
    triggerChildrenResized parentId
    return (file ^. objectId)

registerIx :: (CompositeWidget a, Widget.DisplayObjectClass a) => Int -> WidgetId -> a -> HTMap -> Command UIRegistry.State WidgetId
registerIx i parentId model handlers = do
    file <- UIRegistry.registerIxM i parentId model handlers
    performIO $ Widget.createUI parentId (file ^. objectId) model
    createWidget (file ^. objectId) model
    triggerChildrenResized parentId
    return (file ^. objectId)

registerIx_ :: (CompositeWidget a, Widget.DisplayObjectClass a) => Int -> WidgetId -> a -> HTMap -> Command UIRegistry.State ()
registerIx_ = void .:: registerIx

register_ :: (CompositeWidget a, Widget.DisplayObjectClass a) => WidgetId -> a -> HTMap -> Command UIRegistry.State ()
register_ = void .:. register

update :: (Eq a, CompositeWidget a, Widget.DisplayObjectClass a) => WidgetId -> (a -> a) -> Command UIRegistry.State a
update widgetId fun = do
    UIRegistry.lookupTypedM widgetId >>= \case
        Nothing        -> error $ "update: Widget " <> show widgetId <> " not found or wrong type!"
        Just oldWidget -> do
            newWidget  <- UIRegistry.updateWidgetM widgetId fun
            when ((oldWidget ^. widget) /= newWidget) $ do
                performIO $ Widget.updateUI widgetId (oldWidget ^. widget) newWidget
                updateWidget widgetId (oldWidget ^. widget) newWidget
            return newWidget

update' :: Widget.DisplayObjectClass a => WidgetId -> (a -> a) -> Command UIRegistry.State a
update' = UIRegistry.updateWidgetM

tryUpdate :: (Eq a, CompositeWidget a, Widget.DisplayObjectClass a) => WidgetId -> (a -> a) -> Command UIRegistry.State Bool
tryUpdate widgetId fun = do
    UIRegistry.lookupTypedM widgetId >>= \case
        Nothing        -> return False
        Just oldWidget -> do
            newWidget  <- UIRegistry.updateWidgetM widgetId fun
            when ((oldWidget ^. widget) /= newWidget) $ do
                performIO $ Widget.updateUI widgetId (oldWidget ^. widget) newWidget
                updateWidget widgetId (oldWidget ^. widget) newWidget
            return True

update_ :: (Eq a, CompositeWidget a, Widget.DisplayObjectClass a) => WidgetId -> (a -> a) -> Command UIRegistry.State ()
update_ widgetId fun = void $ update widgetId fun

move :: WidgetId -> Position -> Command UIRegistry.State ()
move widgetId pos = do
    UIRegistry.widgets . ix (fromWidgetId widgetId) . widget . widgetPosition .= pos
    UI.updatePosition' widgetId pos

moveY :: WidgetId -> Double -> Command UIRegistry.State ()
moveY widgetId ny = do
    mayPos <- preuse $ UIRegistry.widgets . ix (fromWidgetId widgetId) . widget . widgetPosition
    withJust mayPos $ \pos -> do
        UIRegistry.widgets . ix (fromWidgetId widgetId) . widget . widgetPosition . y .= ny
        UI.updatePosition' widgetId $ Position (Vector2 (pos ^. x) ny)

moveX :: WidgetId -> Double -> Command UIRegistry.State ()
moveX widgetId nx = do
    mayPos <- preuse $ UIRegistry.widgets . ix (fromWidgetId widgetId) . widget . widgetPosition
    withJust mayPos $ \pos -> do
        UIRegistry.widgets . ix (fromWidgetId widgetId) . widget . widgetPosition . x .= nx
        UI.updatePosition' widgetId $ Position (Vector2 nx (pos ^. y))

moveBy :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
moveBy widgetId vec = do
    mayPos <- preuse $ UIRegistry.widgets . ix (fromWidgetId widgetId) . widget . widgetPosition
    withJust mayPos $ \pos -> do
        UIRegistry.widgets . ix (fromWidgetId widgetId) . widget . widgetPosition .= move pos vec
        mayNewPos <- preuse $ UIRegistry.widgets . ix (fromWidgetId widgetId) . widget . widgetPosition
        withJust mayNewPos $ UI.updatePosition' widgetId

newtype ChildrenResizedHandler = ChildrenResizedHandler (WidgetId -> Command UIRegistry.State ())
triggerChildrenResized :: WidgetId -> Command UIRegistry.State ()
triggerChildrenResized widgetId = do
    let key = TypeKey :: TypeKey ChildrenResizedHandler
    maybeHandler <- handler widgetId key
    withJust maybeHandler $ \(ChildrenResizedHandler handler') -> handler' widgetId

resize :: WidgetId -> Size -> Command UIRegistry.State ()
resize widgetId size = resize' widgetId (const size)

resize' :: WidgetId -> (Size -> Size) -> Command UIRegistry.State ()
resize' = resize'CB True

resizeNoCB :: WidgetId -> (Size -> Size) -> Command UIRegistry.State ()
resizeNoCB = resize'CB False

resize'CB :: Bool -> WidgetId -> (Size -> Size) -> Command UIRegistry.State ()
resize'CB cb widgetId f = do
    UIRegistry.widgets . ix (fromWidgetId widgetId) . widget . widgetSize %= f
    mayWidgetFile <- preuse $ UIRegistry.widgets . ix (fromWidgetId widgetId)
    withJust mayWidgetFile $ \widgetFile -> do
        let model   = widgetFile ^. widget
            wParent = widgetFile ^. Widget.parent
        resizeWidget widgetId (model ^. widgetSize) model
        when cb $ withJust wParent triggerChildrenResized

get :: Widget.DisplayObjectClass a => WidgetId -> Getter a b -> Command UIRegistry.State b
get widgetId f = do
    maybeFile <- UIRegistry.lookupTypedM widgetId
    let file     = fromMaybe (error $ "get: invalid type or widget " <> show widgetId <> "  not exists") maybeFile
    return $ file ^. widget . f

maybeGet :: Widget.DisplayObjectClass a => WidgetId -> Getter a b -> Command UIRegistry.State (Maybe b)
maybeGet widgetId f = do
    maybeFile <- UIRegistry.lookupTypedM widgetId
    return $ view (widget . f) <$> maybeFile

get' :: WidgetId -> Getter Widget.DisplayObject b -> Command UIRegistry.State b
get' widgetId f = do
    maybeFile <- UIRegistry.lookupM widgetId
    let file     = fromMaybe (error $ "get': " <> show widgetId <> " widget not exists") maybeFile
    return $ file ^. widget . f

lookup :: Widget.DisplayObjectClass a => WidgetId -> Command UIRegistry.State a
lookup widgetId = do
    maybeFile <- UIRegistry.lookupTypedM widgetId
    let file   = fromMaybe (error $ "lookup: " <> show widgetId <> "  invalidType") maybeFile
    return $ file ^. widget

children :: WidgetId -> Command UIRegistry.State [WidgetId]
children widgetId = do
    maybeFile <- UIRegistry.lookupM widgetId
    let file   = fromMaybe (error $ "children: " <> show widgetId <> " widget not exists") maybeFile
    return $ file ^. Widget.children

parent :: WidgetId -> Command UIRegistry.State WidgetId
parent widgetId = do
    maybeFile <- UIRegistry.lookupM widgetId
    let file   = fromMaybe (error $ "parent: widget " <> show widgetId <> " not exists") maybeFile
    return $ fromMaybe (error "parent: called on scene") $ file ^. Widget.parent

handler :: Typeable k => WidgetId -> TypeKey k -> Command UIRegistry.State (Maybe k)
handler widgetId k = do
    maybeFile <- UIRegistry.lookupM widgetId
    case maybeFile of
        Nothing   -> return Nothing
        Just file -> return $ HMap.lookup k (file ^. Widget.handlers)

removeWidget :: WidgetId -> Command UIRegistry.State ()
removeWidget widgetId = do
    parentId <- parent widgetId
    -- widgetOver <- use $ UIRegistry.widgetOver
    widgets <- UIRegistry.unregisterM widgetId
    --TODO[react] remove
    -- when (elem widgetId widgets) $ performIO $ Cursor.setCursor Cursor.Normal
    forM_ widgets $ performIO . UI.removeWidget . fromWidgetId
    triggerChildrenResized parentId

newtype LostFocus = LostFocus (WidgetId -> Command Global.State ())
triggerLostFocus :: WidgetId -> Command Global.State ()
triggerLostFocus wid = do
    let key = TypeKey :: (TypeKey LostFocus)
    maybeHandler <- Global.inRegistry $ handler wid key
    withJust maybeHandler $ \(LostFocus handler') -> handler' wid

takeFocus :: WidgetId -> Command Global.State ()
takeFocus wid = do
    currentFocused <- use $ Global.uiRegistry . UIRegistry.focusedWidget
    when (currentFocused /= Just wid) $ do
        withJust currentFocused triggerLostFocus
        Global.uiRegistry . UIRegistry.focusedWidget ?= wid
