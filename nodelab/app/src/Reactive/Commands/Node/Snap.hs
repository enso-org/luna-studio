module Reactive.Commands.Node.Snap
    ( snap
    , snapCoord
    ) where

import           Style.Layout                       (gridSize)
import           Utils.PreludePlus
import           Utils.Vector


snapCoord :: Double -> Double
snapCoord p = fromIntegral . (* gridSize) . round $ p / fromIntegral gridSize

snap :: Vector2 Double -> Vector2 Double
snap (Vector2 x y) = Vector2 x' y' where
  x' = snapCoord x
  y' = snapCoord y
