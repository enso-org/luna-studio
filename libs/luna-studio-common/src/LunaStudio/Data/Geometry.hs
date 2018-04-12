module LunaStudio.Data.Geometry where

import           LunaStudio.Data.Constants (gridSize)
import           LunaStudio.Data.Position  (Position, distanceSquared, fromDoubles, move, vector, x, y)
import           LunaStudio.Data.Vector2   (dotV, scalarProduct)
import           Prologue

type Radius = Double

snapCoord :: Double -> Double
snapCoord p = (* gridSize) . fromIntegral $ (round $ p / gridSize :: Integer)

snap :: Position -> Position
snap = (x %~ snapCoord) . (y %~ snapCoord)

epsilon :: Double
epsilon = 0.005

-- | From Graphics.Gloss.Geometry.Line
-- | Given an infinite line which intersects `P1` and `P1`,
--      return the point on that line that is closest to `P3`
{-# INLINE closestPointOnLine #-}
closestPointOnLine :: (Position, Position) -> Position -> Position
closestPointOnLine line@(p1, p2) p3 = move shift p1 where
    segVec = p2 ^. vector - p1 ^. vector
    u      = closestPointOnLineParam line p3
    shift  = scalarProduct segVec u

-- | From Graphics.Gloss.Geometry.Line
-- | Given an infinite line which intersects P1 and P2,
--      let P4 be the point on the line that is closest to P3.
--
--      Return an indication of where on the line P4 is relative to P1 and P2.
--
-- @
--      if P4 == P1 then 0
--      if P4 == P2 then 1
--      if P4 is halfway between P1 and P2 then 0.5
-- @
--
-- @
--        |
--       P1
--        |
--     P4 +---- P3
--        |
--       P2
--        |
-- @
--
{-# INLINE closestPointOnLineParam #-}
closestPointOnLineParam :: (Position, Position) -> Position -> Double
closestPointOnLineParam (p1, p2) p3
    = (v3 - v1) `dotV` (v2 - v1) / (v2 - v1) `dotV` (v2 - v1) where
        v1 = p1 ^. vector
        v2 = p2 ^. vector
        v3 = p3 ^. vector

doSegmentsIntersect :: (Position, Position) -> (Position, Position) -> Bool
doSegmentsIntersect seg1@(beg1, end1) seg2@(beg2, end2)
    = not (beg1 == end1 || beg2 == end2) && doIntersect where
        isBeg1OnSeg2    = distanceSquared beg1 beg2 + distanceSquared beg1 end2
            == distanceSquared beg2 end2
        isBeg2OnSeg1    = distanceSquared beg2 beg1 + distanceSquared beg2 end1
            == distanceSquared beg1 end1
        leftTop1        = fromDoubles
            (min (beg1 ^. x) (end1 ^. x))
            (min (beg1 ^. y) (end1 ^. y))
        rightBottom1    = fromDoubles
            (max (beg1 ^. x) (end1 ^. x))
            (max (beg1 ^. y) (end1 ^. y))
        leftTop2        = fromDoubles
            (min (beg2 ^. x) (end2 ^. x))
            (min (beg2 ^. y) (end2 ^. y))
        rightBottom2    = fromDoubles
            (max (beg2 ^. x) (end2 ^. x))
            (max (beg2 ^. y) (end2 ^. y))
        isInRectangle p
            =  isPointInRectangle p (leftTop1, rightBottom1)
            && isPointInRectangle p (leftTop2, rightBottom2)
        doIntersect = maybe
            (isBeg1OnSeg2 || isBeg2OnSeg1)
            isInRectangle
            $ intersectLineLine seg1 seg2


-- Line-Line intersection from Graphics.Gloss.Geometry.Line
-- | Given four points specifying two lines, get the point where the two lines
--   cross, if any. Note that the lines extend off to infinity, so the
--   intersection point might not line between either of the two pairs of points.
--
-- @
--     \\      /
--      P1  P4
--       \\ /
--        +
--       / \\
--      P3  P2
--     /     \\
-- @
intersectLineLine :: (Position, Position) -> (Position, Position)
    -> Maybe Position
intersectLineLine (p1, p2) (p3, p4) = do
    let x1   = p1 ^. x
        y1   = p1 ^. y
        x2   = p2 ^. x
        y2   = p2 ^. y
        x3   = p3 ^. x
        y3   = p3 ^. y
        x4   = p4 ^. x
        y4   = p4 ^. y
        dx12 = x1 - x2
        dx34 = x3 - x4
        dy12 = y1 - y2
        dy34 = y3 - y4
        den  = dx12 * dy34  - dy12 * dx34
    if den == 0 then
        Nothing
    else do
        let det12   = x1 * y2 - y1 * x2
            det34   = x3 * y4 - y3 * x4
            numx    = det12 * dx34 - dx12 * det34
            numy    = det12 * dy34 - dy12 * det34
        Just $ fromDoubles (numx / den) (numy / den)

isPointInCircle :: Position -> (Position, Double) -> Bool
isPointInCircle p (circleCenter, radius)
    = distanceSquared p circleCenter <= radius ^ (2 :: Integer)

isPointInRectangle :: Position -> (Position, Position) -> Bool
isPointInRectangle pos (leftTop, rightBottom)
    =  pos ^. x >= leftTop     ^. x - epsilon
    && pos ^. x <= rightBottom ^. x + epsilon
    && pos ^. y >= leftTop     ^. y - epsilon
    && pos ^. y <= rightBottom ^. y + epsilon
