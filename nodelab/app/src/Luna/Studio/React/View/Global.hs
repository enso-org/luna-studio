{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Global where

import           Luna.Studio.Prelude

import           Numeric                            (showFFloat)
import           Luna.Studio.Data.Angle             (Angle)
import           Luna.Studio.Data.Vector


showSvg :: Double -> String
showSvg a = Numeric.showFFloat (Just 1) a "" -- limit Double to two decimal numbers

connectionWidth :: Double
connectionWidth = 3

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
    in  number * t - pi - t + gap/2


portAngleStop :: Int -> Int -> Double -> Angle
portAngleStop num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = portGap r
        t      = portAngle numOfPorts
    in  number * t - pi - gap/2


nodeToNodeAngle :: Double -> Double -> Double -> Double -> Angle
nodeToNodeAngle srcX srcY dstX dstY
    | srcX < dstX = atan ((srcY-dstY) / (srcX-dstX))
    | otherwise   = atan ((srcY-dstY) / (srcX-dstX)) + pi


connectionSrc :: Vector2 Double -> Vector2 Double -> Int -> Int -> Bool -> Vector2 Double
connectionSrc (Vector2 x1 y1) (Vector2 x2 y2) _   _          True =
    let t    = nodeToNodeAngle x1 y1 x2 y2
        srcX = portRadius * cos(t) + x1
        srcY = portRadius * sin(t) + y2
    in  Vector2 srcX srcY
connectionSrc (Vector2 x1 y1) (Vector2 x2 y2) num numOfPorts _    =
    let t      = nodeToNodeAngle x1 y1 x2 y2
        number = num
        ports  = numOfPorts
        srcX   = portRadius * cos(t) + x1
        srcY   = portRadius * sin(t) + y2
    in  Vector2 srcX srcY


connectionDst :: Vector2 Double -> Vector2 Double -> Int -> Int -> Bool -> Vector2 Double
connectionDst (Vector2 _  _ ) (Vector2 x2 y2) _   _          True = Vector2 x2 y2
connectionDst (Vector2 x1 y1) (Vector2 x2 y2) num numOfPorts _    =
    let t      = nodeToNodeAngle x1 y1 x2 y2
        number = num
        ports  = numOfPorts
        dstX   = portRadius * (-cos(t)) + x2
        dstY   = portRadius * (-sin(t)) + y2
    in  Vector2 dstX dstY


{-
x1' = fromString $ showSvg $ portRadius * cos(t) + x1
y1' = fromString $ showSvg $ portRadius * sin(t) + y1
x2' = fromString $ showSvg $ portRadius * (-cos(t)) + x2
y2' = fromString $ showSvg $ portRadius * (-sin(t)) + y2
-}
