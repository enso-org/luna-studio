module Luna.Studio.Data.Angle where

import           Data.Fixed
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude

type Angle  = Double

normAngle :: Angle -> Angle
normAngle a = (2 * pi + a) `mod'` (2 * pi)

toRelAngle :: Angle -> Angle
toRelAngle a = if a > pi then (2 * pi) - a else a

angleDiff :: Angle -> Angle -> Angle
angleDiff a1 a2 = toRelAngle . normAngle $ a2 - a1

toAngle :: Vector2 Double -> Angle
toAngle (Vector2 0.0 0.0) = 0.0
toAngle (Vector2 x y) = normAngle $ atan2 y x

boundedAngle :: Double -> Int -> Position -> Position -> Double
boundedAngle centralAngle' count src dst = angle where
    mult = case count of
        1 -> 0.8
        2 -> 0.7
        3 -> 0.6
        _ -> 0.5
    span' = pi / (fromIntegral count) / 2.0 * mult
    centralAngle = centralAngle'
    minAngle = centralAngle - span'
    maxAngle = centralAngle + span'
    connAngle = toAngle (dst - src)
    diffAngle = (normAngle $ (centralAngle' - connAngle + pi)) - pi
    angle
        | diffAngle < (-span') = maxAngle
        | diffAngle > span'    = minAngle
        | otherwise            = connAngle
