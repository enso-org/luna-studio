module Luna.Studio.Action.Geometry.Node
    ( nodeToNodeAngle
    ) where

import           Luna.Studio.Data.Angle  (Angle)
import           Luna.Studio.Data.Vector (Position, x, y)
import           Luna.Studio.Prelude


nodeToNodeAngle :: Position -> Position -> Angle
nodeToNodeAngle src dst =
    let srcX = src ^. x
        srcY = src ^. y
        dstX = dst ^. x
        dstY = dst ^. y
    in  if srcX < dstX
        then atan ((srcY - dstY) / (srcX - dstX))
        else atan ((srcY - dstY) / (srcX - dstX)) + pi
