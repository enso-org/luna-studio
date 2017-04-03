{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.State.Model.Connection where

import           Control.Monad.Trans.Maybe                     (MaybeT (MaybeT), runMaybeT)
import           Data.Position                                 (Position, distanceSquared, move, y)
import           Data.Vector                                   (Vector2 (Vector2))
import           Luna.Studio.Action.Command                    (Command)
import           Luna.Studio.Action.State.Model.ExpressionNode (nodeToNodeAngle)
import           Luna.Studio.Action.State.Model.Port           (portAngleStart, portAngleStop, portGap)
import           Luna.Studio.Action.State.Model.Sidebar        (getInputSidebarPortPosition, getOutputSidebarPortPosition)
import           Luna.Studio.Action.State.NodeEditor           (getConnection, getConnections, getNode, getPort)
import           Luna.Studio.Data.Color                        (Color (Color))
import           Luna.Studio.Data.Geometry                     (closestPointOnLine, closestPointOnLineParam, doesSegmentsIntersects)
import           Luna.Studio.Data.PortRef                      (AnyPortRef (OutPortRef'), InPortRef, OutPortRef)
import qualified Luna.Studio.Data.PortRef                      as PortRef
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection            (Connection (Connection), ConnectionId,
                                                                CurrentConnection (CurrentConnection), Mode (Normal, Sidebar), connectionId,
                                                                containsNode, toConnection)
import qualified Luna.Studio.React.Model.Connection            as Model
import           Luna.Studio.React.Model.Constants             (lineHeight, nodeExpandedWidth, nodeRadius, portRadius)
import           Luna.Studio.React.Model.Node                  (Node (Expression), NodeLoc, hasPort)
import qualified Luna.Studio.React.Model.Node                  as Node
import           Luna.Studio.React.Model.Node.ExpressionNode   (ExpressionNode, countArgPorts, countOutPorts, isCollapsed, nodeLoc,
                                                                position)
import           Luna.Studio.React.Model.Node.SidebarNode      (isInputSidebar)
import           Luna.Studio.React.Model.Port                  (Port, PortId (InPortId, OutPortId), color, getPortNumber, isSelf, portId)
import           Luna.Studio.State.Global                      (State)


-- WARNING: Since getInputSidebar and getOutputSidebar can change scene redrawConnectionForSidebarNodes may be needed if connection is made for sidebar nodes

createConnectionModel :: OutPortRef -> InPortRef -> Command State (Maybe Connection)
createConnectionModel srcPortRef dstPortRef = runMaybeT $ do
    let srcNodeLoc = srcPortRef ^. PortRef.srcNodeLoc
        dstNodeLoc = dstPortRef ^. PortRef.dstNodeLoc
        dstPortId  = InPortId $ dstPortRef ^. PortRef.dstPortId
    srcNode <- MaybeT $ getNode srcNodeLoc
    dstNode <- MaybeT $ getNode dstNodeLoc
    srcPort <- MaybeT $ getPort srcPortRef
    if hasPort dstPortId dstNode then do
        dstPort <- MaybeT $ getPort dstPortRef
        ((srcPos, dstPos), t) <- MaybeT $ getConnectionPositionAndMode srcNode srcPort dstNode dstPort
        return $ Connection srcPortRef dstPortRef srcPos dstPos t $ srcPort ^. color
    else if countArgPorts dstNode == getPortNumber dstPortId then case dstNode of
        Expression n -> MaybeT $ fmap2 (toConnection srcPortRef dstPortRef) $
            createCurrentConnectionModel (OutPortRef' srcPortRef) $ n ^. position & y %~ (+30)
        _            -> nothing
    else nothing

createCurrentConnectionModel :: AnyPortRef -> Position -> Command State (Maybe CurrentConnection)
createCurrentConnectionModel portRef mousePos = runMaybeT $ do
    let pid = portRef ^. PortRef.portId
    node                 <- MaybeT $ getNode $ portRef ^. PortRef.nodeLoc
    (connPortPos, t, c)  <-
        if hasPort pid node then do
            port             <- MaybeT $ getPort portRef
            (connPortPos, t) <- MaybeT $ getCurrentConnectionSrcPositionAndMode node port mousePos
            return (connPortPos, t, port ^. color)
        else if countArgPorts node == getPortNumber pid then case node of
            Expression n -> return (n ^. position & y %~ (+30), Normal, Color 0)
            _            -> nothing
        else nothing
    return $ CurrentConnection connPortPos mousePos t c

distSqFromMouseIfIntersect :: NodeLoc -> Position -> ConnectionId -> Command State (Maybe (ConnectionId, Double))
distSqFromMouseIfIntersect nl nodePos connId = runMaybeT $ do
    conn <- MaybeT $ getConnection connId
    if containsNode nl conn then nothing else do
        let srcPos  = conn ^. Model.srcPos
            dstPos  = conn ^. Model.dstPos
            proj    = closestPointOnLine (srcPos, dstPos) nodePos
            u       = closestPointOnLineParam (srcPos, dstPos) nodePos
            distSq  = distanceSquared proj nodePos
        if u < 0 || u > 1 || distSq > nodeRadius ^ (2 :: Integer)
            then nothing
            else return (connId, distSq)

getIntersectingConnections :: ExpressionNode -> Position -> Command State (Maybe ConnectionId)
getIntersectingConnections node mousePos = do
    let nl           = node ^. nodeLoc
        posToCompare = if isCollapsed node then node ^. position else mousePos
    connIds             <- map (view connectionId) <$> getConnections
    intersecingConnIds' <- forM connIds $ distSqFromMouseIfIntersect nl posToCompare
    let intersecingConnIds = catMaybes intersecingConnIds'
    return $ if null intersecingConnIds then Nothing else
        Just $ fst $ minimumBy (\(_, distSq1) (_, distSq2) -> compare distSq1 distSq2) intersecingConnIds

getConnectionsIntersectingSegment :: (Position, Position) -> Command State [ConnectionId]
getConnectionsIntersectingSegment seg = flip fmap getConnections $
    map (view Model.connectionId) . filter (
        \conn -> doesSegmentsIntersects seg (conn ^. Model.srcPos, conn ^. Model.dstPos) )

getConnectionPositionAndMode :: Node -> Port -> Node -> Port -> Command State (Maybe ((Position, Position), Mode))
getConnectionPositionAndMode (Node.Sidebar _) srcPort (Node.Sidebar _) dstPort = runMaybeT $ do
    srcConnPos <- MaybeT $ getInputSidebarPortPosition srcPort
    dstConnPos <- MaybeT $ getOutputSidebarPortPosition dstPort
    return ((srcConnPos, dstConnPos), Sidebar)
getConnectionPositionAndMode (Node.Sidebar _) srcPort dstNode dstPort = runMaybeT $ do
    srcConnPos <- MaybeT $ getInputSidebarPortPosition srcPort
    dstConnPos <- MaybeT $ fmap2 fst $ getCurrentConnectionSrcPositionAndMode dstNode dstPort srcConnPos
    return ((srcConnPos, dstConnPos), Sidebar)
getConnectionPositionAndMode srcNode srcPort (Node.Sidebar _) dstPort = runMaybeT $ do
    dstConnPos <- MaybeT $ getOutputSidebarPortPosition dstPort
    srcConnPos <- MaybeT $ fmap2 fst $ getCurrentConnectionSrcPositionAndMode srcNode srcPort dstConnPos
    return ((srcConnPos, dstConnPos), Sidebar)
getConnectionPositionAndMode (Expression srcNode) srcPort (Expression dstNode) dstPort = do
    let srcPos     = srcNode ^. position
        dstPos     = dstNode ^. position
        isSrcExp   = not . isCollapsed $ srcNode
        isDstExp   = not . isCollapsed $ dstNode
        srcPortNum = getPortNumber $ srcPort ^. portId
        dstPortNum = getPortNumber $ dstPort ^. portId
        numOfSrcOutPorts = countOutPorts srcNode
        numOfDstInPorts  = countArgPorts dstNode
        srcConnPos = connectionSrc srcPos dstPos isSrcExp isDstExp srcPortNum numOfSrcOutPorts $ countOutPorts srcNode + countArgPorts srcNode == 1
        dstConnPos = connectionDst srcPos dstPos isSrcExp isDstExp dstPortNum numOfDstInPorts $ isSelf $ dstPort ^. portId
    return $ Just ((srcConnPos, dstConnPos), Normal)

getCurrentConnectionSrcPositionAndMode :: Node -> Port -> Position -> Command State (Maybe (Position, Mode))
getCurrentConnectionSrcPositionAndMode (Node.Sidebar node) port _ = do
    if isInputSidebar node
        then fmap2 (, Sidebar) $ getInputSidebarPortPosition  port
        else fmap2 (, Sidebar) $ getOutputSidebarPortPosition port
getCurrentConnectionSrcPositionAndMode (Expression node) port mousePos =
    return . Just $ case port ^. portId of
        OutPortId _ -> (, Normal) $ connectionSrc pos mousePos isExp False portNum numOfSameTypePorts $ countOutPorts node + countArgPorts node == 1
        InPortId  _ -> (, Normal) $ connectionDst mousePos pos False isExp portNum numOfSameTypePorts $ isSelf $ port ^. portId
    where
        pos                = node ^. position
        isExp              = not . isCollapsed $ node
        portNum            = getPortNumber $ port ^. portId
        numOfSameTypePorts = case port ^. portId of
            (InPortId  _) -> countArgPorts node
            (OutPortId _) -> countOutPorts node

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
        g  = portGap portRadius / 4

-- TODO[JK]: dst numOfInputs
connectionSrc :: Position -> Position -> Bool -> Bool -> Int -> Int -> Bool -> Position
connectionSrc src dst isSrcExpanded _isDstExpanded num numOfSameTypePorts isSingle =
    if isSrcExpanded then move (Vector2 nodeExpandedWidth 0) src
    else move (Vector2 (portRadius * cos t) (portRadius * sin t)) src where
        t = if isSingle then
                 nodeToNodeAngle src dst
            else getConnectionAngle src dst num numOfSameTypePorts

connectionDst :: Position -> Position -> Bool -> Bool -> Int -> Int -> Bool -> Position
connectionDst src dst isSrcExpanded isDstExpanded num numOfSameTypePorts isSelf' =
    if isSelf'
        then dst
    else if isDstExpanded
        then move (Vector2 0 (lineHeight * (fromIntegral num + 1))) dst
        else move (Vector2 (portRadius * (-cos t)) (portRadius * (-sin t))) dst where
            src' = if isSrcExpanded then move (Vector2 nodeExpandedWidth 0) src else src
            t    = getConnectionAngle src' dst num numOfSameTypePorts
