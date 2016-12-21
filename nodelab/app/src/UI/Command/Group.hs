{-# LANGUAGE Rank2Types #-}

module UI.Command.Group where

import           Luna.Studio.Data.Vector         (Position, Vector2 (Vector2), x, y)
import           Luna.Studio.Prelude

import           Luna.Studio.Commands.Command    (Command)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import qualified Luna.Studio.State.UIRegistry    as UIRegistry
import           Object.Widget                   (WidgetId, widgetPosition, widgetSize)

import           Style.Types                     (Padding (..))

maximum' :: [Double] -> Double
maximum' [] = 0.0
maximum' xs = maximum xs

getFarEdge :: Getter (Position) Double -> WidgetId -> Command UIRegistry.State Double
getFarEdge getter wid = do
    offset <- UICmd.get' wid $ widgetPosition . getter
    size   <- UICmd.get' wid $ widgetSize     . getter
    return $ offset + size

updateSize :: Padding -> WidgetId -> Command UIRegistry.State ()
updateSize (Padding top right bottom left) wid = do
    widgets <- UICmd.children wid
    widths  <- mapM (getFarEdge x) widgets
    heights <- mapM (getFarEdge y) widgets
    if length widgets == 0 then
        UICmd.resize wid $ Vector2 0 0
    else
        UICmd.resize wid $ Vector2 (left + right + maximum' widths) (top + bottom + maximum' heights)
