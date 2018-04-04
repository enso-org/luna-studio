module LunaStudio.Data.Angle where

import           Data.Fixed
import           LunaStudio.Data.Vector2
import           Prologue


type Angle  = Double

normAngle :: Angle -> Angle
normAngle a = (2 * pi + a) `mod'` (2 * pi)

toRelAngle :: Angle -> Angle
toRelAngle a = if a > pi then (2 * pi) - a else a

angleDiff :: Angle -> Angle -> Angle
angleDiff a1 a2 = toRelAngle . normAngle $ a2 - a1

toAngle :: Vector2 Double -> Angle
toAngle (Vector2 0.0 0.0) = 0.0
toAngle (Vector2 x' y')   = normAngle $ atan2 y' x'

--TODO toAngle v = if v == mempty then def else normAngle $ atan2 y x

