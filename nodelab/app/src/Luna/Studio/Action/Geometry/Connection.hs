{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.Geometry.Connection
    ( createConnectionModel
    , createCurrentConnectionModel
    ) where

import qualified Data.Map.Lazy                         as Map
import           Data.Position                         (Position, Vector2 (Vector2), move)
import           Empire.API.Data.Connection            (Connection)
import qualified Empire.API.Data.Connection            as Connection
import           Empire.API.Data.PortRef               (AnyPortRef (InPortRef', OutPortRef'))
import qualified Empire.API.Data.PortRef               as PortRef
import           Luna.Studio.Action.Camera.Screen      (getScreenLeftCenter, getScreenRightCenter, getScreenRightCenter,
                                                        translateToWorkspace)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.Geometry.Constants (grid, lineHeight, nodeExpandedWidth, portRadius)
import           Luna.Studio.Action.Geometry.Node      (nodeToNodeAngle)
import           Luna.Studio.Action.Geometry.Port      (countSameTypePorts, getPortNumber, isPortSelf, isPortSingle, portAngleStart,
                                                        portAngleStop, portGap)
import           Luna.Studio.Action.Graph.Lookup       (getPort)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Connection    as Model
import           Luna.Studio.React.Model.Node          (Node, isInputEdge, isOutputEdge)
import qualified Luna.Studio.React.Model.Node          as Node
import           Luna.Studio.React.Model.Port          (Port)
import qualified Luna.Studio.React.Model.Port          as Port
import           Luna.Studio.State.Global              (State, getNode)


getConnectionAngle :: Position -> Position -> Int -> Int -> Double
getConnectionAngle srcPos dstPos num numOfSameTypePorts =
    if      t' > a' - pi / 2 - g then a - pi / 2 - g
    else if t' < b' - pi / 2 + g then b - pi / 2 + g
    else t where
        a  = portAngleStop  num numOfSameTypePorts portRadius
        b  = portAngleStart num numOfSameTypePorts portRadius
        t  = nodeToNodeAngle srcPos dstPos
        a' = if a < pi then a + (2 * pi) else a
        b' = if b < pi then b + (2 * pi) else b
        t' = if t < pi then t + (2 * pi) else t
        g  = portGap portRadius / 4

-- TODO[JK]: dst numOfInputs
connectionSrc :: Position -> Position -> Bool -> Bool -> Int -> Int -> Bool -> Position
connectionSrc src dst isSrcExpanded _isDstExpanded num numOfSameTypePorts isSingle =
    if isSrcExpanded then
         move (Vector2 nodeExpandedWidth 0) src
    else move (Vector2 (portRadius * cos t) (portRadius * sin t)) src where
        t = if isSingle then
                 nodeToNodeAngle src dst
            else getConnectionAngle src dst num numOfSameTypePorts

connectionDst :: Position -> Position -> Bool -> Bool -> Int -> Int -> Bool -> Position
connectionDst src dst isSrcExpanded isDstExpanded num numOfSameTypePorts isSelf =
    if isSelf then dst
    else if isDstExpanded then
         move (Vector2 0 (lineHeight * (fromIntegral num + 1))) dst
    else move (Vector2 (portRadius * (-cos t)) (portRadius * (-sin t))) dst where
        src' = if isSrcExpanded then move (Vector2 nodeExpandedWidth 0) src else src
        t    = getConnectionAngle src' dst num numOfSameTypePorts

--TODO: Find out real position of port
getInputEdgePortPosition :: Int -> Command State Position
getInputEdgePortPosition portNumber =
    move (Vector2 grid (portNumber' * grid)) <$> getScreenLeftCenter >>= translateToWorkspace where
        portNumber' = fromIntegral portNumber

getOutputEdgePortPosition :: Int -> Bool -> Command State Position
getOutputEdgePortPosition portNumber isSelf =
    move (Vector2 (-grid) (portNumber' * grid)) <$> getScreenRightCenter >>= translateToWorkspace where
        portNumber' = fromIntegral $ if isSelf then 0 else  portNumber + 1

getConnectionPosition :: Node -> Port -> Node -> Port -> Command State (Position, Position)
getConnectionPosition srcNode srcPort dstNode dstPort = do
    let srcPos     = srcNode ^. Node.position
        dstPos     = dstNode ^. Node.position
        isSrcExp   = srcNode ^. Node.isExpanded
        isDstExp   = dstNode ^. Node.isExpanded
        srcPortNum = getPortNumber srcPort
        dstPortNum = getPortNumber dstPort
        srcPorts   = Map.elems $ srcNode ^. Node.ports
        dstPorts   = Map.elems $ dstNode ^. Node.ports
        numOfSrcOutPorts = countSameTypePorts srcPort srcPorts
        numOfDstInPorts  = countSameTypePorts dstPort dstPorts
    srcConnPos <- if isInputEdge srcNode then
            getInputEdgePortPosition srcPortNum
        else return $ connectionSrc srcPos dstPos isSrcExp isDstExp srcPortNum numOfSrcOutPorts $ isPortSingle srcPort srcPorts
    dstConnPos <- if isOutputEdge dstNode then
            getOutputEdgePortPosition dstPortNum $ isPortSelf dstPort
        else return $ connectionDst srcPos dstPos isSrcExp isDstExp dstPortNum numOfDstInPorts $ isPortSelf dstPort
    return (srcConnPos, dstConnPos)

getCurrentConnectionSrcPosition :: Node -> Port -> Position -> Command State Position
getCurrentConnectionSrcPosition node port mousePos = if isInputEdge node then
        getInputEdgePortPosition  portNum
    else if isOutputEdge node then
        getOutputEdgePortPosition portNum $ isPortSelf port
    else return $ case port ^. Port.portRef of
        OutPortRef' _ -> connectionSrc pos mousePos isExp False portNum numOfSameTypePorts $ isPortSingle port ports
        InPortRef'  _ -> connectionDst mousePos pos False isExp portNum numOfSameTypePorts $ isPortSelf port
    where
        pos                = node ^. Node.position
        isExp              = node ^. Node.isExpanded
        portNum            = getPortNumber port
        ports              = Map.elems $ node ^. Node.ports
        numOfSameTypePorts = countSameTypePorts port ports

createConnectionModel :: Connection -> Command State (Maybe Model.Connection)
createConnectionModel connection = do
    let srcPortRef = connection ^. Connection.src
        dstPortRef = connection ^. Connection.dst
    maySrcNode <- getNode $ srcPortRef ^. PortRef.srcNodeId
    mayDstNode <- getNode $ dstPortRef ^. PortRef.dstNodeId
    maySrcPort <- getPort srcPortRef
    mayDstPort <- getPort dstPortRef

    case (maySrcNode, maySrcPort, mayDstNode, mayDstPort) of
        (Just srcNode, Just srcPort, Just dstNode, Just dstPort) -> do
            (srcPos, dstPos) <- getConnectionPosition srcNode srcPort dstNode dstPort
            let color = srcPort ^. Port.color
            return $ Just (Model.Connection dstPortRef srcPos dstPos color)
        _ -> return Nothing

createCurrentConnectionModel :: AnyPortRef -> Position -> Command State (Maybe Model.CurrentConnection)
createCurrentConnectionModel portRef mousePos = do
    mayNode <- getNode $ portRef ^. PortRef.nodeId
    mayPort <- getPort portRef

    case (mayNode, mayPort) of
        (Just node, Just port) -> do
            connPortPos <- getCurrentConnectionSrcPosition node port mousePos
            let color = port ^. Port.color
            return $ Just (Model.CurrentConnection connPortPos mousePos color)
        _ -> return Nothing
