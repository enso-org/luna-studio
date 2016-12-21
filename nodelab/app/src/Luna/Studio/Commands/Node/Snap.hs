module Luna.Studio.Commands.Node.Snap
    ( snap
    , snapCoord
    ) where

import           Luna.Studio.Data.Vector (Position, Vector2 (Vector2))
import           Luna.Studio.Prelude
import           Style.Layout            (gridSize)


snapCoord :: Double -> Double
snapCoord p = fromIntegral . (* gridSize) . round $ p / fromIntegral gridSize

snap :: Position -> Position
snap (Vector2 x y) = Vector2 x' y' where
  x' = snapCoord x
  y' = snapCoord y
