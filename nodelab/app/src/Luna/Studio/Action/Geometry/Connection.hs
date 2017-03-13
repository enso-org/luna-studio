{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.Geometry.Connection
    ( createConnectionModel
    , createCurrentConnectionModel
    , getOutputEdgePortPosition
    , getInputEdgePortPosition
    ) where


import           Control.Monad.Trans.Maybe             (MaybeT (MaybeT), runMaybeT)
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
        a  = portAngleStop  True num numOfSameTypePorts portRadius
        b  = portAngleStart True num numOfSameTypePorts portRadius
        t  = nodeToNodeAngle srcPos dstPos
        a' = if a < pi then a + (2 * pi) else a
        b' = if b < pi then b + (2 * pi) else b
        t' = if t < pi then t + (2 * pi) else t
        g  = portGap portRadius

-- TODO[JK]: dst numOfInputs
connectionSrc :: Position -> Position -> Bool -> Bool -> Int -> Int -> Bool -> Position
connectionSrc src dst isSrcExpanded _isDstExpanded num numOfSameTypePorts isSingle =
    if isSrcExpanded then move (Vector2 nodeExpandedWidth 0) src
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
getInputEdgePortPosition portNumber = runMaybeT $ do
    inputSidebar <- MaybeT getInputSidebar
    let pos = inputSidebar ^. Scene.inputSidebarPosition
        siz = inputSidebar ^. Scene.inputSidebarSize
        posX = pos ^. x + siz ^. x
        posY = (fromIntegral portNumber) * gridSize + pos ^. y + siz ^. y / 2
    lift $ translateToWorkspace (ScreenPosition $ Vector2 posX posY)

getOutputEdgePortPosition :: Int -> Bool -> Command State (Maybe Position)
getOutputEdgePortPosition portNumber isSelf = runMaybeT $ do
    outputSidebar <- MaybeT getOutputSidebar
    let pos = outputSidebar ^. Scene.outputSidebarPosition
        siz = outputSidebar ^. Scene.outputSidebarSize
        posX = pos ^. x
        posY = (fromIntegral $ if isSelf then 0 else  portNumber + 1) * gridSize + pos ^. y + siz ^. y / 2
    lift $ translateToWorkspace (ScreenPosition $ Vector2 posX posY)

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
    if isInputEdge srcNode && isOutputEdge dstNode then runMaybeT $ do
        srcConnPos <- MaybeT $ getInputEdgePortPosition srcPortNum
        dstConnPos <- MaybeT $ getOutputEdgePortPosition dstPortNum $ isPortSelf dstPort
        return (srcConnPos, dstConnPos)
    else if isInputEdge srcNode then runMaybeT $ do
        srcConnPos <- MaybeT $ getInputEdgePortPosition srcPortNum
        dstConnPos <- MaybeT $ getCurrentConnectionSrcPosition dstNode dstPort srcConnPos
        return (srcConnPos, dstConnPos)
    else if isOutputEdge dstNode then runMaybeT $ do
        dstConnPos <- MaybeT $ getOutputEdgePortPosition dstPortNum $ isPortSelf dstPort
        srcConnPos <- MaybeT $ getCurrentConnectionSrcPosition srcNode srcPort dstConnPos
        return (srcConnPos, dstConnPos)
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
createConnectionModel connection = runMaybeT $ do
    let srcPortRef = connection ^. Connection.src
        dstPortRef = connection ^. Connection.dst
    srcNode <- MaybeT $ getNode $ srcPortRef ^. PortRef.srcNodeId
    dstNode <- MaybeT $ getNode $ dstPortRef ^. PortRef.dstNodeId
    srcPort <- MaybeT $ getPort srcPortRef
    dstPort <- MaybeT $ getPort dstPortRef
    if (srcPort ^. Port.visible && dstPort ^. Port.visible) then do
            (srcPos, dstPos) <- MaybeT $ getConnectionPosition srcNode srcPort dstNode dstPort
            return $ Model.Connection dstPortRef srcPos dstPos (srcPort ^. Port.color)
        else nothing

createCurrentConnectionModel :: AnyPortRef -> Position -> Command State (Maybe Model.CurrentConnection)
createCurrentConnectionModel portRef mousePos = runMaybeT $ do
    node        <- MaybeT $ getNode $ portRef ^. PortRef.nodeId
    port        <- MaybeT $ getPort portRef
    connPortPos <- MaybeT $ getCurrentConnectionSrcPosition node port mousePos
    return $ Model.CurrentConnection connPortPos mousePos (port ^. Port.color)
