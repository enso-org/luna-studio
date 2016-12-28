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
import qualified Object.Widget.Port           as Port


type IsSingle = Bool
type IsSelf   = Bool

showSvg :: Double -> String
showSvg a = Numeric.showFFloat (Just 2) a "" -- limit Double to two decimal numbers

transformMatrix :: String -> String -> String -> String
transformMatrix scale offsetX offsetY = "matrix(" <> scale <> " , 0, 0, " <> scale <> " , " <> offsetX <> " , " <> offsetY <> " )"

transformTranslate:: String -> String ->  String
transformTranslate offsetX offsetY = "matrix( 1 , 0, 0, 1, " <> offsetX <> " , " <> offsetY <> " )"

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
    in  number * t + pi - t + gap/2


portAngleStop :: Int -> Int -> Double -> Angle
portAngleStop num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = portGap r
        t      = portAngle numOfPorts
    in  number * t + pi - gap/2


nodeToNodeAngle :: Position -> Position -> Angle
nodeToNodeAngle src dst =
    let srcX = src ^. x
        srcY = src ^. y
        dstX = dst ^. x
        dstY = dst ^. y
    in  if srcX < dstX
        then atan ((srcY - dstY) / (srcX - dstX))
        else atan ((srcY - dstY) / (srcX - dstX)) + pi


connectionSrc :: Position -> Position -> Int -> Int -> IsSingle -> Position
connectionSrc src dst _ _ True =
    let t  = nodeToNodeAngle src dst
        x' = portRadius * cos t + src ^. x
        y' = portRadius * sin t + src ^. y
    in  Position (Vector2 x' y')
connectionSrc src dst num numOfPorts _    =
    let -- a  = portAngleStart num numOfPorts portRadius
        -- b  = portAngleStop  num numOfPorts portRadius
        t  = nodeToNodeAngle src dst
        x'  = portRadius * cos t + src ^. x
        y'  = portRadius * sin t + src ^. y
    in  Position (Vector2 x' y')
    
connectionDst :: Position -> Position -> Int -> Int -> IsSelf -> Position
connectionDst src dst _   _          True = dst
connectionDst src dst num numOfPorts _    =
    let a  = portAngleStop  num numOfPorts portRadius
        b  = portAngleStart num numOfPorts portRadius
        t  = nodeToNodeAngle src dst

        a' = a -- if a < pi/2 then a + 2 * pi else a
        b' = b
        t' = t --if t < pi then t + pi else t - pi

        t''= if t' - pi > a'
             then a' - pi
             else if t' - pi < b'
             then b' - pi
             else t'

        x' = portRadius * (-cos t'') + dst ^. x
        y' = portRadius * (-sin t'') + dst ^. y
    in  Position (Vector2 x' y')


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


getConnectionPosition :: NodeId -> PortId -> NodeId -> PortId -> Command State (Maybe (Position, Position))
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

getCurrentConnectionSrcPosition :: NodeId -> PortId -> Position -> Command State (Maybe Position)
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
