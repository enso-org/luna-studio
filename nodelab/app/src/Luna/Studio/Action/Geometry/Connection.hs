module Luna.Studio.Action.Geometry.Connection
    ( getConnectionPosition
    , getConnectionSrcPosition
    ) where

import qualified Data.Map.Lazy                         as Map
import           Empire.API.Data.PortRef               (AnyPortRef, InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef               as PortRef
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.Geometry.Constants (portRadius)
import           Luna.Studio.Action.Geometry.Node      (nodeToNodeAngle)
import           Luna.Studio.Action.Geometry.Port      (IsSelf, IsSingle, countSameTypePorts, getPortNumber, isPortSelf, isPortSingle,
                                                        portAngleStart, portAngleStop, portGap)
import           Luna.Studio.Action.Graph.Lookup       (getNode, getPort)
import           Luna.Studio.Data.Vector               (Position (Position), Vector2 (Vector2), x, y)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node          as Node
import           Luna.Studio.State.Global              (State)


connectionSrc :: Position -> Position -> Bool -> Bool -> Int -> Int -> IsSingle -> Position
connectionSrc src dst isSrcExpanded isDstExpanded _ _ True =
    let t  = nodeToNodeAngle src dst
        x' = portRadius * cos t + src ^. x
        y' = portRadius * sin t + src ^. y
    in  Position (Vector2 x' y')
connectionSrc src dst isSrcExpanded isDstExpanded num numOfPorts _    =
    let a  = portAngleStop  num numOfPorts portRadius
        b  = portAngleStart num numOfPorts portRadius
        t  = nodeToNodeAngle src dst
        a' = if a < pi then a + (2 * pi) else a
        b' = if b < pi then b + (2 * pi) else b
        t' = if t < pi then t + (2 * pi) else t
        g  = portGap portRadius / 4

        t''= if t' > a'- pi/2 - g then a - pi/2 - g else
             if t' < b'- pi/2 + g then b - pi/2 + g else t --TODO: determine why the pi/2 offset is necessary
        x' = portRadius * cos t'' + src ^. x
        y' = portRadius * sin t'' + src ^. y
    in  Position (Vector2 x' y')


connectionDst :: Position -> Position -> Bool -> Bool -> Int -> Int -> IsSelf -> Position
connectionDst src dst isSrcExpanded isDstExpanded _   _          True = dst
connectionDst src dst isSrcExpanded isDstExpanded num numOfPorts _    =
    let a  = portAngleStop  num numOfPorts portRadius
        b  = portAngleStart num numOfPorts portRadius
        t  = nodeToNodeAngle src dst

        a' = if a < pi then a + (2 * pi) else a
        b' = if b < pi then b + (2 * pi) else b
        t' = if t < pi then t + (2 * pi) else t

        g = portGap portRadius / 4

        t''= if t' > a'- pi/2 - g then a - pi/2 - g else
             if t' < b'- pi/2 + g then b - pi/2 + g else t --TODO: determine why the pi/2 offset is necessary
        x' = portRadius * (-cos t'') + dst ^. x
        y' = portRadius * (-sin t'') + dst ^. y
    in  Position (Vector2 x' y')


getConnectionPosition :: OutPortRef -> InPortRef -> Command State (Maybe (Position, Position))
getConnectionPosition srcPortRef dstPortRef = do
    maySrcNode <- getNode $ srcPortRef ^. PortRef.srcNodeId
    mayDstNode <- getNode $ dstPortRef ^. PortRef.dstNodeId
    maySrcPort <- getPort srcPortRef
    mayDstPort <- getPort dstPortRef

    case (maySrcNode, maySrcPort, mayDstNode, mayDstPort) of
        (Just srcNode, Just srcPort, Just dstNode, Just dstPort) -> do
            let srcPorts      = Map.elems $ srcNode ^. Node.ports
                dstPorts      = Map.elems $ dstNode ^. Node.ports
                srcPos        = srcNode ^. Node.position
                dstPos        = dstNode ^. Node.position
                isSrcExpanded = srcNode ^. Node.isExpanded
                isDstExpanded = dstNode ^. Node.isExpanded
                srcConnPos    = connectionSrc srcPos dstPos isSrcExpanded isDstExpanded (getPortNumber srcPort) (countSameTypePorts srcPort srcPorts) (isPortSingle srcPort srcPorts)
                dstConnPos    = connectionDst srcPos dstPos isSrcExpanded isDstExpanded (getPortNumber dstPort) (countSameTypePorts dstPort dstPorts) (isPortSelf dstPort)
            return $ Just (srcConnPos, dstConnPos)
        _ -> return Nothing


getConnectionSrcPosition :: AnyPortRef -> Position -> Command State (Maybe Position)
getConnectionSrcPosition srcPortRef dstPos = do
    maySrcNode <- getNode $ srcPortRef ^. PortRef.nodeId
    maySrcPort <- getPort srcPortRef

    case (maySrcNode, maySrcPort) of
        (Just srcNode, Just srcPort) -> do
            let srcPorts      = Map.elems $ srcNode ^. Node.ports
                srcPos        = srcNode ^. Node.position
                isSrcExpanded = srcNode ^. Node.isExpanded
                srcConnPos    = connectionSrc srcPos dstPos isSrcExpanded False (getPortNumber srcPort) (countSameTypePorts srcPort srcPorts) (isPortSingle srcPort srcPorts)
            return $ Just srcConnPos
        _ -> return Nothing
