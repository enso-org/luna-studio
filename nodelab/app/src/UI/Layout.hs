{-# LANGUAGE Rank2Types #-}
module UI.Layout where

import           Luna.Studio.Prelude            hiding (lens)

import           Control.Monad                (foldM)
import           Data.HMap.Lazy               (HTMap)
import           Luna.Studio.Data.Vector

import           Object.Widget                (WidgetId, widgetSize, widgetVisible)
import           Luna.Studio.Commands.Command    (Command)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import           Luna.Studio.State.UIRegistry    (addHandler)
import qualified Luna.Studio.State.UIRegistry    as UIRegistry

import qualified Object.Widget.Group          as Group
import qualified UI.Command.Group             as Group
import qualified UI.Handlers.Group            as Group
import           UI.Widget.Group              ()


getHeight :: WidgetId -> Command UIRegistry.State (WidgetId, Double, Bool)
getHeight wid = do
    size <- UICmd.get' wid widgetSize
    vis <- UICmd.get' wid widgetVisible
    return $ (wid, size ^. y, vis)

getWidth :: WidgetId -> Command UIRegistry.State (WidgetId, Double, Bool)
getWidth wid = do
    size <- UICmd.get' wid widgetSize
    vis <- UICmd.get' wid widgetVisible
    return $ (wid, size ^. x, vis)

moveY :: Double -> Double -> (WidgetId, Double, Bool) -> Command UIRegistry.State Double
moveY spacing offset (wid, height, True) = do
    UICmd.moveY wid offset
    return $ offset + spacing + height
moveY _      offset (_   , _    , False) =
    return $ offset

moveX :: Double -> Double -> (WidgetId, Double, Bool) -> Command UIRegistry.State Double
moveX spacing offset (wid, width, True) = do
    UICmd.moveX wid offset
    return $ offset + spacing + width
moveX _       offset (_  , _   , False) =
    return $ offset

verticalLayoutHandler :: Double -> HTMap
verticalLayoutHandler spacing = addHandler (UICmd.ChildrenResizedHandler $ verticalLayoutHandler' spacing) mempty

verticalLayoutHandler' :: Double -> WidgetId -> Command UIRegistry.State ()
verticalLayoutHandler' spacing wid = do
    verticalLayout spacing wid
    maybePadding <- UICmd.maybeGet wid $ Group.style . Group.padding
    let padding = fromMaybe def maybePadding
    Group.updateSize padding wid

verticalLayout :: Double -> WidgetId -> Command UIRegistry.State ()
verticalLayout spacing wid = do
    widgets <- UICmd.children wid
    heights <- mapM getHeight widgets
    void $ foldM (moveY spacing) 0.0 heights

horizontalLayoutHandler :: Double -> HTMap
horizontalLayoutHandler spacing = addHandler (UICmd.ChildrenResizedHandler $ horizontalLayoutHandler' spacing True) mempty

horizontalLayoutHandlerNoResize :: Double -> HTMap
horizontalLayoutHandlerNoResize spacing = addHandler (UICmd.ChildrenResizedHandler $ horizontalLayoutHandler' spacing False) mempty

horizontalLayoutHandler' :: Double -> Bool -> WidgetId -> Command UIRegistry.State ()
horizontalLayoutHandler' spacing resize wid = do
    horizontalLayout spacing wid
    maybePadding <- UICmd.maybeGet wid $ Group.style . Group.padding
    let padding = fromMaybe def maybePadding
    when resize $ Group.updateSize padding wid

horizontalLayout :: Double -> WidgetId -> Command UIRegistry.State ()
horizontalLayout spacing wid = do
    widgets <- UICmd.children wid
    heights <- mapM getWidth widgets
    void $ foldM (moveX spacing) 0.0 heights

flexVerticalLayoutHandler, flexHorizontalLayoutHandler :: Double -> HTMap
flexVerticalLayoutHandler   spacing = addHandler (Group.WidgetResizedHandler $ flexVerticalLayout   spacing) mempty
flexHorizontalLayoutHandler spacing = addHandler (Group.WidgetResizedHandler $ flexHorizontalLayout spacing) mempty

flexVerticalLayout, flexHorizontalLayout :: Double -> WidgetId -> Vector2 Double -> Command UIRegistry.State ()
flexVerticalLayout   = flexLayout y
flexHorizontalLayout = flexLayout x

flexLayout :: Lens' (Vector2 Double) Double -> Double -> WidgetId -> Vector2 Double -> Command UIRegistry.State ()
flexLayout lens spacing wid size = do
    let height = size ^. lens
    widgets <- UICmd.children wid
    let newHeight = height / (fromIntegral $ length widgets)
    forM_ widgets $ \wid' -> UICmd.resizeNoCB wid' (lens .~ newHeight)
    verticalLayout spacing wid
