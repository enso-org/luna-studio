{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Global where

import qualified Data.Map.Lazy                as Map
import           Empire.API.Data.Node         (NodeId)
import           Empire.API.Data.Port         (InPort (..), OutPort (..), PortId (..))
import           Empire.API.Data.PortRef      (toAnyPortRef)
import           Luna.Studio.Commands.Command (Command)
import           Luna.Studio.Commands.Graph   (getNode, getPort)
import           Luna.Studio.Data.Angle       (Angle)
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global     (State)
import           Numeric                      (showFFloat)
import qualified Object.Widget.Node           as Node
import           Object.Widget.Port           (Port (..))

type IsSingle = Bool
type IsSelf   = Bool

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

connectionSrc :: Vector2 Double -> Vector2 Double -> Int -> Int -> IsSingle -> Vector2 Double
connectionSrc (Vector2 x1 y1) (Vector2 x2 y2) _ _ True =
    let t    = nodeToNodeAngle x1 y1 x2 y2
        srcX = portRadius * cos(t) + x1
        srcY = portRadius * sin(t) + y1
    in  Vector2 srcX srcY
-- FIXME: implement port limits
connectionSrc (Vector2 x1 y1) (Vector2 x2 y2) num numOfPorts False =
    let t      = nodeToNodeAngle x1 y1 x2 y2
        number = num
        ports  = numOfPorts
        srcX   = portRadius * cos(t) + x1
        srcY   = portRadius * sin(t) + y1
    in  Vector2 srcX srcY

connectionDst :: Vector2 Double -> Vector2 Double -> Int -> Int -> IsSelf -> Vector2 Double
connectionDst (Vector2 _  _ ) (Vector2 x2 y2) _ _ True = Vector2 x2 y2
-- FIXME: implement port limits
connectionDst (Vector2 x1 y1) (Vector2 x2 y2) num numOfPorts False =
    let t      = nodeToNodeAngle x1 y1 x2 y2
        number = num
        ports  = numOfPorts
        dstX   = portRadius * (-cos(t)) + x2
        dstY   = portRadius * (-sin(t)) + y2
    in  Vector2 dstX dstY

isIn :: Port -> Int
isIn (Port _ (InPortId (Arg _)) _ _) = 1
isIn _ = 0

isOut :: Port -> Int
isOut (Port _ (OutPortId _) _ _) = 1
isOut _ = 0

countInPorts :: [Port] -> Int
countInPorts ports = foldl (\acc p -> acc + (isIn p)) 0 ports

countOutPorts :: [Port] -> Int
countOutPorts ports = foldl (\acc p -> acc + (isOut p)) 0 ports

countPorts :: [Port] -> Int
countPorts ports = (countInPorts ports) + (countOutPorts ports)

countSameTypePorts :: Port -> [Port] -> Int
countSameTypePorts (Port _ (InPortId _)  _ _) = countInPorts
countSameTypePorts (Port _ (OutPortId _) _ _) = countOutPorts

getPortNumber :: Port -> Int
getPortNumber (Port _ (InPortId  (Arg i))        _ _) = i
getPortNumber (Port _ (OutPortId (Projection i)) _ _) = i
getPortNumber _ = 0

isPortSingle :: Port -> [Port] -> Bool
isPortSingle (Port _ (OutPortId All) _ _) ports = countPorts ports == 1
isPortSingle _ _ = False

isPortSelf :: Port -> Bool
isPortSelf (Port _ (InPortId Self) _ _) = True
isPortSelf _ = False


getConnectionPosition :: NodeId -> PortId -> NodeId -> PortId -> Command State (Maybe (Vector2 Double, Vector2 Double))
getConnectionPosition srcNodeId srcPortId dstNodeId dstPortId = do
    maySrcNode <- getNode srcNodeId
    mayDstNode <- getNode dstNodeId
    maySrcPort <- getPort $ toAnyPortRef srcNodeId srcPortId
    mayDstPort <- getPort $ toAnyPortRef dstNodeId dstPortId

    case (maySrcNode, maySrcPort, mayDstNode, mayDstPort) of
        (Just srcNode, Just srcPort, Just dstNode, Just dstPort) -> do
            let srcPorts   = Map.elems $ srcNode ^. Node.ports
                dstPorts   = Map.elems $ dstNode ^. Node.ports
                srcPos     = srcNode ^. Node.position
                dstPos     = dstNode ^. Node.position
                srcConnPos = connectionSrc srcPos dstPos (getPortNumber srcPort) (countSameTypePorts srcPort srcPorts) (isPortSingle srcPort srcPorts)
                dstConnPos = connectionDst srcPos dstPos (getPortNumber dstPort) (countSameTypePorts dstPort dstPorts) (isPortSelf dstPort)
            return $ Just (srcConnPos, dstConnPos)
        _ -> return Nothing

getCurrentConnectionSrcPosition :: NodeId -> PortId -> Vector2 Double -> Command State (Maybe (Vector2 Double))
getCurrentConnectionSrcPosition srcNodeId srcPortId dstPos = do
    maySrcNode <- getNode srcNodeId
    maySrcPort <- getPort $ toAnyPortRef srcNodeId srcPortId

    case (maySrcNode, maySrcPort) of
        (Just srcNode, Just srcPort) -> do
            let srcPorts   = Map.elems $ srcNode ^. Node.ports
                srcPos     = srcNode ^. Node.position
                srcConnPos = connectionSrc srcPos dstPos (getPortNumber srcPort) (countSameTypePorts srcPort srcPorts) (isPortSingle srcPort srcPorts)
            return $ Just srcConnPos
        _ -> return Nothing


{-
x1' = fromString $ showSvg $ portRadius * cos(t) + x1
y1' = fromString $ showSvg $ portRadius * sin(t) + y1
x2' = fromString $ showSvg $ portRadius * (-cos(t)) + x2
y2' = fromString $ showSvg $ portRadius * (-sin(t)) + y2
-}
