module Luna.Studio.Action.State.Model.Port where

import           Luna.Studio.Data.Angle            (Angle)
import           Luna.Prelude
import           Luna.Studio.React.Model.Constants (nodeRadius)


portGap :: Double -> Angle
portGap r = 0.2 * nodeRadius / r -- to avoid gap narrowing

portAngle :: Int -> Angle
portAngle numOfPorts = pi / fromIntegral numOfPorts

portAngleStart :: Bool -> Int -> Int -> Double -> Angle
portAngleStart isShape num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = if isShape then (portGap r)/2 else 0
        t      = portAngle numOfPorts
    in  pi - number * t + gap

portAngleStop :: Bool -> Int -> Int -> Double -> Angle
portAngleStop isShape num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = if isShape then (portGap r)/2 else 0
        t      = portAngle numOfPorts
    in  pi - number * t + t - gap
