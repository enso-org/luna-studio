{-# LANGUAGE DeriveAnyClass #-}
module Data.Curve where

import           Common.Prelude
import           Control.DeepSeq          (NFData)
import           Data.Timestamp           (Timestamp)
import           LunaStudio.Data.Position (Position (Position), vector)
import           LunaStudio.Data.Vector2  (scalarProduct)


data CurveSegment = CurveSegment { _segmentBegin :: Position
                                 , _controlPoint :: Position
                                 , _segmentEnd   :: Position
                                 , _approved     :: Bool
                                 } deriving (Eq, Show, Generic, NFData)

makeLenses ''CurveSegment

data Curve = Curve { _segments     :: [CurveSegment]
                   , _lastUpdate   :: Maybe Timestamp
                   , _lastVelocity :: Maybe Double
                   } deriving (Eq, Show, Generic, NFData)

makeLenses ''Curve


getPointOnCurveSegment :: CurveSegment -> Double -> Position
getPointOnCurveSegment curveSegment t = Position result where
    beg    = curveSegment ^. segmentBegin . vector
    cP     = curveSegment ^. controlPoint . vector
    end    = curveSegment ^. segmentEnd . vector
    result = scalarProduct beg ((1 - t) * (1 - t)) + scalarProduct cP (2 * (1 - t) * t) + scalarProduct end (t * t)


getPointsOnCurveSegment :: CurveSegment -> Int -> [Position]
getPointsOnCurveSegment _ 0 = []
getPointsOnCurveSegment curveSegment numOfPoints = map (getPointOnCurveSegment curveSegment) ts where
    t  = 1 / fromIntegral (numOfPoints + 1)
    ts = map ((*t) . fromIntegral) [1..numOfPoints]
