module Luna.Studio.Action.Geometry.Node
    ( nodeToNodeAngle
    , IsExpanded
    ) where

import           Data.Position          (Position, x, y)
import           Luna.Studio.Data.Angle (Angle)
import           Luna.Studio.Prelude

type IsExpanded = Bool

nodeToNodeAngle :: Position -> Position -> Angle
nodeToNodeAngle src dst =
    let srcX = src ^. x
        srcY = src ^. y
        dstX = dst ^. x
        dstY = dst ^. y
    in  if srcX < dstX
        then atan ((srcY - dstY) / (srcX - dstX))
        else atan ((srcY - dstY) / (srcX - dstX)) + pi
