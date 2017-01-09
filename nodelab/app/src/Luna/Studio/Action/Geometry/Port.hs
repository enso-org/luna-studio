module Luna.Studio.Action.Geometry.Port
    ( IsSelf
    , IsSingle
    , countSameTypePorts
    , getPortNumber
    , isPortSelf
    , isPortSingle
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


type IsSingle = Bool
type IsSelf   = Bool

portGap :: Double -> Angle
portGap r = 0.2 * nodeRadius / r -- to avoid gap narrowing

portAngle :: Int -> Angle
portAngle numOfPorts = pi / fromIntegral numOfPorts

portAngleStart :: Int -> Int -> Double -> Angle
portAngleStart num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = portGap r
        t      = portAngle numOfPorts
    in  pi - number * t + gap/2 --number * t + pi - t + gap/2


portAngleStop :: Int -> Int -> Double -> Angle
portAngleStop num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = portGap r
        t      = portAngle numOfPorts
    in  pi - number * t + t - gap/2 --number * t + pi - gap/2

countInput :: Port -> Int
countInput port = case port ^. Port.portId of
    InPortId (Arg _) -> 1
    _                -> 0

countOutput :: Port -> Int
countOutput port = case port ^. Port.portId of
    OutPortId _ -> 1
    _           -> 0

countInputs :: [Port] -> Int
countInputs  ports = foldl (\acc p -> acc + (countInput  p)) 0 ports

countOutputs :: [Port] -> Int
countOutputs ports = foldl (\acc p -> acc + (countOutput p)) 0 ports

countPorts :: [Port] -> Int
countPorts ports = (countInputs ports) + (countOutputs ports)

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
