module Luna.Studio.Action.Node.Snap
    ( snap
    , snapCoord
    ) where

import           Data.Position       (Position, x, y)
import           Luna.Studio.Prelude
import           Style.Layout        (gridSize)


snapCoord :: Double -> Double
snapCoord p = fromIntegral . (* gridSize) . round $ p / fromIntegral gridSize

snap :: Position -> Position
snap = (x %~ snapCoord) . (y %~ snapCoord)
