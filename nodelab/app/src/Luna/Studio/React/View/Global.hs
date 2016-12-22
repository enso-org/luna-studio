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
showSvg a = Numeric.showFFloat (Just 1) a "" -- limit Double to two decimal numbers

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
    in  number * t - pi - t + gap/2


portAngleStop :: Int -> Int -> Double -> Angle
portAngleStop num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = portGap r
        t      = portAngle numOfPorts
    in  number * t - pi - gap/2

nodeToNodeAngle :: Double -> Double -> Double -> Double -> Angle
nodeToNodeAngle srcX srcY dstX dstY
    | srcX < dstX = atan ((srcY - dstY) / (srcX - dstX))
    | otherwise   = atan ((srcY - dstY) / (srcX - dstX)) + pi


connectionSrc :: Position -> Position -> Int -> Int -> IsSingle -> Position
connectionSrc (Vector2 x1 y1) (Vector2 x2 y2) _   _          True =
    let t      = nodeToNodeAngle x1 y1 x2 y2
        srcX   = portRadius/2 * cos t + x1 --FIXME: why portRadius is doubled?
        srcY   = portRadius/2 * sin t + y1
    in  Vector2 srcX srcY
connectionSrc (Vector2 x1 y1) (Vector2 x2 y2) num numOfPorts _    =
    let t      = nodeToNodeAngle x1 y1 x2 y2
        number = num
        ports  = numOfPorts
        srcX   = portRadius/2 * cos(t) + x1
        srcY   = portRadius/2 * sin(t) + y1
    in  Vector2 srcX srcY -- FIXME: implement port limits


connectionDst :: Position -> Position -> Int -> Int -> IsSelf -> Position
connectionDst (Vector2 _  _ ) (Vector2 x2 y2) _   _          True = Vector2 x2 y2
connectionDst (Vector2 x1 y1) (Vector2 x2 y2) num numOfPorts _    =
    let t      = nodeToNodeAngle x1 y1 x2 y2
        number = num
        ports  = numOfPorts
        dstX   = portRadius * (-cos(t)) + x2 -- FIXME: implement port limits
        dstY   = portRadius * (-sin(t)) + y2
    in  Vector2 dstX dstY




isIn :: Port -> Int
isIn port = case port ^. Port.portId of
    InPortId (Arg _) -> 1
    _                -> 0

maybeInput :: Port -> Maybe Int
maybeInput port = case port ^. Port.portId of
    InPortId (Arg _) -> Just 1
    _                -> Nothing

isOut :: Port -> Int
isOut port = case port ^. Port.portId of
    OutPortId _ -> 1
    _           -> 0

countInputs :: [Port] -> Int
countInputs ports = foldl (\acc p -> acc + (isIn p)) 0 ports

countOutputs :: [Port] -> Int
countOutputs ports = foldl (\acc p -> acc + (isOut p)) 0 ports

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
