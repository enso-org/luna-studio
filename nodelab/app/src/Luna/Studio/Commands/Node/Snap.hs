module Luna.Studio.Commands.Node.Snap
    ( snap
    , snapCoord
    ) where

import           Luna.Studio.Data.Vector (Position, x, y)
import           Luna.Studio.Prelude
import           Style.Layout            (gridSize)


snapCoord :: Double -> Double
snapCoord p = fromIntegral . (* gridSize) . round $ p / fromIntegral gridSize

snap :: Position -> Position
snap = (x %~ snapCoord) . (y %~ snapCoord)
