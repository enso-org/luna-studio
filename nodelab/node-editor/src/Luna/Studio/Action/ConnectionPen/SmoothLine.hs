module Luna.Studio.Action.ConnectionPen.SmoothLine
    ( addPointToCurve
    , beginCurve
    , curveToSvgPath
    ) where

import           Data.Curve          (Curve (Curve), CurveSegment (CurveSegment))
import qualified Data.Curve          as Curve
import           Data.Position       (Position (Position), distance, vector, x, y)
import           Data.Timestamp      (Timestamp)
import           Data.Vector2         (scalarProduct)
import           Luna.Prelude
import           Prologue            (unwrap)


fingerDragSmoothing :: Bool
fingerDragSmoothing = True

fingerDragSmoothingCoef, fingerDragSmoothingStrength, maxSmoothSeg, minSmoothSeg, smoothCoef :: Double
fingerDragSmoothingCoef = 1
fingerDragSmoothingStrength = 1
maxSmoothSeg = 100
minSmoothSeg = 10
smoothCoef = 50

curveToSvgPath :: Curve -> String
curveToSvgPath curve = do
    let pointToString :: Position -> String
        pointToString p = show (p ^. x) <> " " <> show (p ^. y)
        curveSegmentToString :: CurveSegment -> String
        curveSegmentToString seg = "M" <> segBeg <> " Q " <> controlP <> ", " <> segEnd where
            segBeg   = pointToString $ seg ^. Curve.segmentBegin
            controlP = pointToString $ seg ^. Curve.controlPoint
            segEnd   = pointToString $ seg ^. Curve.segmentEnd
        segmentStrings = map curveSegmentToString $ curve ^. Curve.segments
    foldl (<>) "" $ intersperse " " segmentStrings

addSegmentToCurve :: Curve -> Position -> Timestamp -> Maybe Double -> Double -> Curve
addSegmentToCurve curve controlPoint timestamp mayVelocity minLengthToApprove = do
    let approvedSegments = dropWhile (not . view Curve.approved) $ curve ^. Curve.segments
    if null (curve ^. Curve.segments) || null approvedSegments then
        Curve [CurveSegment controlPoint controlPoint controlPoint True] (Just timestamp) mayVelocity
    else do
        let (lastSegment : unchangableSegments) = approvedSegments
            lastControlPoint = lastSegment ^. Curve.controlPoint
            segmentBegin = Position (scalarProduct (lastControlPoint ^. vector + controlPoint ^. vector) 0.5)
            isApproved   = distance lastControlPoint controlPoint >= minLengthToApprove
            newSegment = CurveSegment segmentBegin controlPoint controlPoint isApproved
            updatedPrevSegment = lastSegment & Curve.segmentEnd .~ segmentBegin
        Curve (newSegment : updatedPrevSegment : unchangableSegments) (Just timestamp) mayVelocity

getCurrentVelocity :: Timestamp -> Timestamp -> Position -> Position -> Maybe Double -> Double
getCurrentVelocity prevTimestamp currentTimestamp prevPos currentPos mayLastVelocity = do
    let timeDiff = fromIntegral $ unwrap currentTimestamp - unwrap prevTimestamp
        dist     = distance prevPos currentPos
    if fingerDragSmoothing && isJust mayLastVelocity then do
        let lastVelocity = fromJust mayLastVelocity
            acceleration = (fingerDragSmoothingCoef * (dist / timeDiff - lastVelocity))
                         / (fingerDragSmoothingStrength * timeDiff + 1 - fingerDragSmoothingStrength)
        lastVelocity + acceleration
    else dist / timeDiff

addPointToCurve :: Position -> Timestamp -> Curve -> Curve
addPointToCurve pos timestamp curve =
    if isNothing (curve ^. Curve.lastUpdate) || null (curve ^. Curve.segments) then
        addSegmentToCurve curve pos timestamp Nothing 0
    else do
        let prevTimestamp   = fromJust $ curve ^. Curve.lastUpdate
            lastPoint       = view Curve.controlPoint $ head $ curve ^. Curve.segments
            currentVelocity = getCurrentVelocity prevTimestamp timestamp lastPoint pos (curve ^. Curve.lastVelocity)
            minLengthToApprove = min maxSmoothSeg $ max minSmoothSeg $ smoothCoef * sqrt currentVelocity
        addSegmentToCurve curve pos timestamp (Just currentVelocity) minLengthToApprove

beginCurve :: Position -> Timestamp -> Curve
beginCurve pos ts = addPointToCurve pos ts (Curve [] Nothing Nothing)
