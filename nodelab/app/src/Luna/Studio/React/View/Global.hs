{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Global where

import           Data.Matrix                  (Matrix)
import qualified Data.Matrix                  as Matrix
import           Empire.API.Data.Port         (InPort (..), OutPort (..), PortId (..))
import           Luna.Studio.Data.Angle       (Angle)
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Port (Port (..))
import qualified Luna.Studio.React.Model.Port as Port
import           Numeric                      (showFFloat)


type IsSingle = Bool
type IsSelf   = Bool

showSvg :: Double -> String
showSvg a = Numeric.showFFloat (Just 4) a "" -- limit Double to two decimal numbers

transformMatrix :: String -> String -> String -> String
transformMatrix scale offsetX offsetY = "matrix(" <> scale <> " , 0, 0, " <> scale <> " , " <> offsetX <> " , " <> offsetY <> " )"

transformTranslate :: String -> String ->  String
transformTranslate offsetX offsetY = "matrix( 1 , 0, 0, 1, " <> offsetX <> " , " <> offsetY <> " )"

showTransformMatrix :: Show a => Matrix a -> String
showTransformMatrix matrix = (foldl (<>) "matrix3d(" $ intersperse ", " $ map show $ Matrix.toList matrix) <> ")"

connectionWidth :: Double
connectionWidth = 2.6

lineHeight :: Double
lineHeight = 16

nodeExpandedWidth :: Double
nodeExpandedWidth = 224

nodeRadius :: Double
nodeRadius = 20

nodeRadius' :: Double
nodeRadius' = nodeRadius - connectionWidth

portRadius :: Double
portRadius  = nodeRadius - connectionWidth/2

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


nodeToNodeAngle :: Position -> Position -> Angle
nodeToNodeAngle src dst =
    let srcX = src ^. x
        srcY = src ^. y
        dstX = dst ^. x
        dstY = dst ^. y
    in  if srcX < dstX
        then atan ((srcY - dstY) / (srcX - dstX))
        else atan ((srcY - dstY) / (srcX - dstX)) + pi

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
