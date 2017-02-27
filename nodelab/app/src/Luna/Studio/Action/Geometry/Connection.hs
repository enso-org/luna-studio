
{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.Geometry.Connection
    ( createConnectionModel
    , createCurrentConnectionModel
    , getOutputEdgePortPosition
    , getInputEdgePortPosition
    ) where

import qualified Data.Map.Lazy                         as Map
import           Data.Position                         (Position, Vector2 (Vector2), move)
import           Data.ScreenPosition                   (ScreenPosition (ScreenPosition), x, y)
import           Empire.API.Data.Connection            (Connection)
import qualified Empire.API.Data.Connection            as Connection
import           Empire.API.Data.PortRef               (AnyPortRef (InPortRef', OutPortRef'))
import qualified Empire.API.Data.PortRef               as PortRef
import qualified JS.Scene                              as Scene
import           Luna.Studio.Action.Camera.Screen      (getInputSidebar, getOutputSidebar, translateToWorkspace)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.Geometry.Constants (gridSize, lineHeight, nodeExpandedWidth, portRadius)
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

getInputEdgePortPosition :: Int -> Command State (Maybe Position)
getInputEdgePortPosition portNumber = getInputSidebar >>= \mayInputSidebar ->
    case mayInputSidebar of
        Just inputSidebar -> fmap Just $ translateToWorkspace (ScreenPosition $ Vector2 posX posY) where
            pos = inputSidebar ^. Scene.inputSidebarPosition
            siz = inputSidebar ^. Scene.inputSidebarSize
            posX = pos ^. x + siz ^. x
            posY = (fromIntegral portNumber) * gridSize + pos ^. y + siz ^. y / 2
        Nothing -> return Nothing

getOutputEdgePortPosition :: Int -> Bool -> Command State (Maybe Position)
getOutputEdgePortPosition portNumber isSelf = getOutputSidebar >>= \mayOutputSidebar ->
    case mayOutputSidebar of
        Just outputSidebar -> fmap Just $ translateToWorkspace (ScreenPosition $ Vector2 posX posY) where
            pos = outputSidebar ^. Scene.outputSidebarPosition
            siz = outputSidebar ^. Scene.outputSidebarSize
            posX = pos ^. x
            posY = (fromIntegral $ if isSelf then 0 else  portNumber + 1) * gridSize + pos ^. y + siz ^. y / 2
        Nothing -> return Nothing

getConnectionPosition :: Node -> Port -> Node -> Port -> Command State (Maybe (Position, Position))
getConnectionPosition srcNode srcPort dstNode dstPort = do
    let srcPos     = srcNode ^. Node.position
        dstPos     = dstNode ^. Node.position
        isSrcExp   = not $ srcNode ^. Node.isCollapsed
        isDstExp   = not $ dstNode ^. Node.isCollapsed
        srcPortNum = getPortNumber srcPort
        dstPortNum = getPortNumber dstPort
        srcPorts   = Map.elems $ srcNode ^. Node.ports
        dstPorts   = Map.elems $ dstNode ^. Node.ports
        numOfSrcOutPorts = countSameTypePorts srcPort srcPorts
        numOfDstInPorts  = countSameTypePorts dstPort dstPorts
    if isInputEdge srcNode && isOutputEdge dstNode then do
        maySrcConnPos <- getInputEdgePortPosition srcPortNum
        mayDstConnPos <- getOutputEdgePortPosition dstPortNum $ isPortSelf dstPort
        return $ (,) <$> maySrcConnPos <*> mayDstConnPos
    else if isInputEdge srcNode then do
        maySrcConnPos <- getInputEdgePortPosition srcPortNum
        mayDstConnPos <- case maySrcConnPos of
            Just srcConnPos -> getCurrentConnectionSrcPosition dstNode dstPort srcConnPos
            Nothing         -> return Nothing
        return $ (,) <$> maySrcConnPos <*> mayDstConnPos
    else if isOutputEdge dstNode then do
        mayDstConnPos <- getOutputEdgePortPosition dstPortNum $ isPortSelf dstPort
        maySrcConnPos <- case mayDstConnPos of
            Just dstConnPos -> getCurrentConnectionSrcPosition srcNode srcPort dstConnPos
            Nothing -> return Nothing
        return $ (,) <$> maySrcConnPos <*> mayDstConnPos
    else do
        let srcConnPos = connectionSrc srcPos dstPos isSrcExp isDstExp srcPortNum numOfSrcOutPorts $ isPortSingle srcPort srcPorts
            dstConnPos = connectionDst srcPos dstPos isSrcExp isDstExp dstPortNum numOfDstInPorts $ isPortSelf dstPort
        return $ Just (srcConnPos, dstConnPos)


getCurrentConnectionSrcPosition :: Node -> Port -> Position -> Command State (Maybe Position)
getCurrentConnectionSrcPosition node port mousePos = if isInputEdge node then
        getInputEdgePortPosition  portNum
    else if isOutputEdge node then
        getOutputEdgePortPosition portNum $ isPortSelf port
    else return $ Just $ case port ^. Port.portRef of
        OutPortRef' _ -> connectionSrc pos mousePos isExp False portNum numOfSameTypePorts $ isPortSingle port ports
        InPortRef'  _ -> connectionDst mousePos pos False isExp portNum numOfSameTypePorts $ isPortSelf port
    where
        pos                = node ^. Node.position
        isExp              = not $ node ^. Node.isCollapsed
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
        (Just srcNode, Just srcPort, Just dstNode, Just dstPort) ->
            if srcPort ^. Port.visible && dstPort ^. Port.visible then do
                    mayConnPos <- getConnectionPosition srcNode srcPort dstNode dstPort
                    case mayConnPos of
                        Just (srcPos, dstPos) -> return $ Just (Model.Connection dstPortRef srcPos dstPos (srcPort ^. Port.color))
                        Nothing -> return Nothing
                else return Nothing
        _ -> return Nothing

createCurrentConnectionModel :: AnyPortRef -> Position -> Command State (Maybe Model.CurrentConnection)
createCurrentConnectionModel portRef mousePos = do
    mayNode <- getNode $ portRef ^. PortRef.nodeId
    mayPort <- getPort portRef

    case (mayNode, mayPort) of
        (Just node, Just port) -> do
            mayConnPortPos <- getCurrentConnectionSrcPosition node port mousePos
            case mayConnPortPos of
                Just connPortPos -> return $ Just (Model.CurrentConnection connPortPos mousePos (port ^. Port.color))
                Nothing -> return Nothing
        _ -> return Nothing
