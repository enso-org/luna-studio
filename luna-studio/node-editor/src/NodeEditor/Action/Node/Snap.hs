module NodeEditor.Action.Node.Snap
    ( snap
    , snapCoord
    ) where

import           Data.Position                     (Position, x, y)
import           Common.Prelude
import           NodeEditor.React.Model.Constants (gridSize)

snapCoord :: Double -> Double
snapCoord p = (* gridSize) . fromIntegral $ (round $ p / gridSize :: Integer)

snap :: Position -> Position
snap = (x %~ snapCoord) . (y %~ snapCoord)
