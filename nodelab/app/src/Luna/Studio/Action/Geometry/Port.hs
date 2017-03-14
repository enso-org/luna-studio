module Luna.Studio.Action.Geometry.Port
    ( countSameTypePorts
    , getPortNumber
    , isPortSelf
    , isPortSingle
    , isPortInput
    , portAngleStart
    , portAngleStop
    , portGap
    ) where

import           Empire.API.Data.Port                  (InPort (Arg, Self), OutPort (All, Projection), PortId (InPortId, OutPortId))
import           Luna.Studio.Action.Geometry.Constants (nodeRadius)
import           Luna.Studio.Data.Angle                (Angle)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Port          (Port)
import qualified Luna.Studio.React.Model.Port          as Port


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

countInput :: Port -> Int
countInput port = case port ^. Port.portId of
    InPortId (Arg _) -> 1
    _                -> 0

countOutput :: Port -> Int
countOutput port = case port ^. Port.portId of
    OutPortId _ -> 1
    _           -> 0

countInputs :: [Port] -> Int
countInputs = foldl (\acc p -> acc + countInput  p) 0

countOutputs :: [Port] -> Int
countOutputs = foldl (\acc p -> acc + countOutput p) 0

countPorts :: [Port] -> Int
countPorts ports = countInputs ports + countOutputs ports

countSameTypePorts :: Port -> [Port] -> Int
countSameTypePorts port = case port ^. Port.portId of
    InPortId  _ -> countInputs
    OutPortId _ -> countOutputs

getPortNumber :: Port -> Int
getPortNumber port = case port ^. Port.portId of
    InPortId  (Arg i)        -> i
    OutPortId (Projection i) -> i
    _                        -> 0

isPortSingle :: Port -> [Port] -> Bool
isPortSingle port ports = case port ^. Port.portId of
    OutPortId All -> countPorts ports == 1
    _             -> False

isPortSelf :: Port -> Bool
isPortSelf port = case port ^. Port.portId of
    InPortId Self -> True
    _             -> False

isPortInput :: Port -> Bool
isPortInput p = case p ^. Port.portId of
    InPortId _ -> True
    _          -> False
