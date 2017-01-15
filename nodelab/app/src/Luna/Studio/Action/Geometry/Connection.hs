module Luna.Studio.Action.Geometry.Connection
    ( getConnectionPosition
    , getCurrentConnectionPosition
    ) where

import qualified Data.Map.Lazy                         as Map
import           Empire.API.Data.PortRef               (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef               as PortRef
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.Geometry.Constants (portRadius, nodeExpandedWidth, lineHeight)
import           Luna.Studio.Action.Geometry.Node      (nodeToNodeAngle)
import           Luna.Studio.Action.Geometry.Port      (IsSelf, IsSingle, countSameTypePorts, getPortNumber, isPortSelf, isPortSingle,
                                                        portAngleStart, portAngleStop, portGap)
import           Luna.Studio.Action.Graph.Lookup       (getNode, getPort)
import           Luna.Studio.Data.Vector               (Position (Position), Vector2 (Vector2), x, y)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node          as Node
import           Luna.Studio.State.Global              (State)


connectionSrc :: Position -> Position -> Bool -> Bool -> Int -> Int -> IsSingle -> Position
connectionSrc src dst isSrcExpanded _             _   _          True =
    let t  = nodeToNodeAngle src dst
        srcExpX = src ^. x + nodeExpandedWidth
        srcExpY = src ^. y
        srcX = if isSrcExpanded then srcExpX else portRadius * cos t + src ^. x
        srcY = if isSrcExpanded then srcExpY else portRadius * sin t + src ^. y
    in  Position (Vector2 srcX srcY)

connectionSrc src dst isSrcExpanded isDstExpanded dstInputNum numOfDstInputs _    =
    let srcExpX = src ^. x + nodeExpandedWidth
        srcExpY = src ^. y
        dstExpX = dst ^. x
        dstExpY = dst ^. y -- TODO: numOfInputs
        trueSrc = if isSrcExpanded then Position(Vector2 srcExpX srcExpY) else src
        trueDst = if isDstExpanded then Position(Vector2 dstExpX dstExpY) else dst
        a  = portAngleStop  dstInputNum numOfDstInputs portRadius
        b  = portAngleStart dstInputNum numOfDstInputs portRadius
        t  = nodeToNodeAngle trueSrc trueDst
        a' = if a < pi then a + (2 * pi) else a
        b' = if b < pi then b + (2 * pi) else b
        t' = if t < pi then t + (2 * pi) else t
        g  = portGap portRadius / 4
        t''= if t' > a'- pi/2 - g then a - pi/2 - g else
             if t' < b'- pi/2 + g then b - pi/2 + g else t
        srcX = if isSrcExpanded then srcExpX else portRadius * cos t'' + src ^. x
        srcY = if isSrcExpanded then srcExpY else portRadius * sin t'' + src ^. y
    in  Position (Vector2 srcX srcY)


connectionDst :: Position -> Position -> Bool -> Bool -> Int -> Int -> IsSelf -> Position
connectionDst _   dst _             _             _   _          True = dst
connectionDst src dst isSrcExpanded isDstExpanded num numOfPorts _    =
    let a   = portAngleStop  num numOfPorts portRadius
        b   = portAngleStart num numOfPorts portRadius
        src'= if isSrcExpanded then Position (Vector2 (src ^. x + nodeExpandedWidth) (src ^. y)) else src
        t   = nodeToNodeAngle src' dst
        a'  = if a < pi then a + (2 * pi) else a
        b'  = if b < pi then b + (2 * pi) else b
        t'  = if t < pi then t + (2 * pi) else t
        g   = portGap portRadius / 4
        t'' = if t' > a'- pi/2 - g then a - pi/2 - g else
              if t' < b'- pi/2 + g then b - pi/2 + g else t
        dstExpX = dst ^. x
        dstExpY = dst ^. y + lineHeight * (fromIntegral num + 1)
        dstX = if isDstExpanded then dstExpX else portRadius * (-cos t'') + dst ^. x
        dstY = if isDstExpanded then dstExpY else portRadius * (-sin t'') + dst ^. y
    in  Position (Vector2 dstX dstY)


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


getCurrentConnectionPosition :: AnyPortRef -> Position -> Command State (Maybe (Position, Position))
getCurrentConnectionPosition portRef mousePos = do
    mayNode <- getNode $ portRef ^. PortRef.nodeId
    mayPort <- getPort portRef

    case (mayNode, mayPort) of
        (Just node, Just port) -> do
            let ports      = Map.elems $ node ^. Node.ports
                pos        = node ^. Node.position
                isExpanded = node ^. Node.isExpanded
                portNum    = getPortNumber port
                numOfPorts = countSameTypePorts port ports
                isSelf     = isPortSelf port
                connPortPos = case portRef of
                    OutPortRef' _ -> connectionSrc pos      mousePos isExpanded False      portNum numOfPorts False
                    InPortRef'  _ -> connectionDst mousePos pos      False      isExpanded portNum numOfPorts isSelf
            return $ Just (connPortPos, mousePos)
        _                     -> return Nothing
