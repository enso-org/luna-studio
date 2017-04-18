{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.State.Model.Connection where

import           Control.Monad.Trans.Maybe                     (MaybeT (MaybeT), runMaybeT)
import           Data.Position                                 (Position, distanceSquared, move, y)
import           Data.Vector                                   (Vector2 (Vector2))
import           Empire.API.Data.PortRef                       (AnyPortRef (OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef                       as PortRef
import           Luna.Studio.Action.Command                    (Command)
import           Luna.Studio.Action.State.Model.ExpressionNode (nodeToNodeAngle)
import           Luna.Studio.Action.State.Model.Port           (portAngleStart, portAngleStop, portGap)
import           Luna.Studio.Action.State.NodeEditor           (getConnection, getConnections, getLayout, getNode, getPort)
import           Luna.Studio.Data.Color                        (Color (Color))
import           Luna.Studio.Data.Geometry                     (closestPointOnLine, closestPointOnLineParam, doesSegmentsIntersects)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection            (Connection (Connection), ConnectionId,
                                                                CurrentConnection (CurrentConnection), Mode (Normal, Sidebar), connectionId,
                                                                containsNode, toConnection)
import qualified Luna.Studio.React.Model.Connection            as Model
import           Luna.Studio.React.Model.Constants             (gridSize, lineHeight, nodeExpandedWidth, nodeRadius, portRadius)
import           Luna.Studio.React.Model.Layout                (Layout, inputSidebarPortPosition, outputSidebarPortPosition)
import           Luna.Studio.React.Model.Node                  (Node (Expression), NodeLoc, hasPort)
import qualified Luna.Studio.React.Model.Node                  as Node
import           Luna.Studio.React.Model.Node.ExpressionNode   (ExpressionNode, countArgPorts, countOutPorts, isCollapsed, nodeLoc,
                                                                position)
import           Luna.Studio.React.Model.Port                  (EitherPort, InPort, OutPort, color, getPortNumber, isSelf, portId)
import           Luna.Studio.State.Global                      (State)


-- WARNING: Since getInputSidebar and getOutputSidebar can change scene redrawConnectionForSidebarNodes may be needed if connection is made for sidebar nodes

createConnectionModel :: OutPortRef -> InPortRef -> Command State (Maybe Connection)
createConnectionModel srcPortRef dstPortRef = runMaybeT $ do
    let srcNodeLoc = srcPortRef ^. PortRef.srcNodeLoc
        dstNodeLoc = dstPortRef ^. PortRef.dstNodeLoc
        dstPortId  = dstPortRef ^. PortRef.dstPortId
    srcNode <- MaybeT $ getNode srcNodeLoc
    dstNode <- MaybeT $ getNode dstNodeLoc
    srcPort <- MaybeT $ getPort srcPortRef
    if hasPort dstPortId dstNode then do
        dstPort <- MaybeT $ getPort dstPortRef
        layout <- lift getLayout
        ((srcPos, dstPos), t) <- MaybeT $ return $ connectionPositionAndMode srcNode srcPort dstNode dstPort layout
        return $ Connection srcPortRef dstPortRef srcPos dstPos t $ srcPort ^. color
    else if countArgPorts dstNode == getPortNumber dstPortId then case dstNode of
        Expression n -> MaybeT $ fmap2 (toConnection srcPortRef dstPortRef) $
            createCurrentConnectionModel (OutPortRef' srcPortRef) $ getPortPhantomPosition n
        _            -> nothing
    else nothing

createCurrentConnectionModel :: AnyPortRef -> Position -> Command State (Maybe CurrentConnection)
createCurrentConnectionModel portRef mousePos = runMaybeT $ do
    let pid = portRef ^. PortRef.portId
    node                 <- MaybeT $ getNode $ portRef ^. PortRef.nodeLoc
    (connPortPos, t, c)  <-
        if hasPort pid node then do
            port             <- MaybeT $ getPort portRef
            layout <- lift getLayout
            (connPortPos, t) <- MaybeT $ return $ currentConnectionSrcPositionAndMode node (convert port) mousePos layout
            return (connPortPos, t, port ^. color)
        else if countArgPorts node == getPortNumber pid then case node of
            Expression n -> return (getPortPhantomPosition n, Normal, Color 0)
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

getPortPhantomPosition :: ExpressionNode -> Position
getPortPhantomPosition n = n ^. position & y %~ (+ 2 * gridSize)

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

connectionPositionAndMode :: Node -> OutPort -> Node -> InPort -> Layout -> Maybe ((Position, Position), Mode)
connectionPositionAndMode (Node.Input _) srcPort (Node.Output _) dstPort layout = do
    srcConnPos <- inputSidebarPortPosition srcPort layout
    dstConnPos <- outputSidebarPortPosition dstPort layout
    return ((srcConnPos, dstConnPos), Sidebar)
connectionPositionAndMode (Node.Input _) srcPort dstNode dstPort layout = do
    srcConnPos <- inputSidebarPortPosition srcPort layout
    dstConnPos <- fst <$> currentConnectionSrcPositionAndMode dstNode (Left dstPort) srcConnPos layout
    return ((srcConnPos, dstConnPos), Sidebar)
connectionPositionAndMode srcNode srcPort (Node.Output _) dstPort layout = do
    dstConnPos <- outputSidebarPortPosition dstPort layout
    srcConnPos <- fst <$> currentConnectionSrcPositionAndMode srcNode (Right srcPort) dstConnPos layout
    return ((srcConnPos, dstConnPos), Sidebar)
connectionPositionAndMode (Expression srcNode) srcPort (Expression dstNode) dstPort _ = do
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
    return ((srcConnPos, dstConnPos), Normal)

currentConnectionSrcPositionAndMode :: Node -> EitherPort -> Position -> Layout -> Maybe (Position, Mode)
currentConnectionSrcPositionAndMode (Node.Input  _  ) (Right port) _ layout = (, Sidebar) <$> inputSidebarPortPosition  port layout
currentConnectionSrcPositionAndMode (Node.Output _  ) (Left  port) _ layout = (, Sidebar) <$> outputSidebarPortPosition port layout
currentConnectionSrcPositionAndMode (Expression node) eport mousePos layout = Just $ case eport of
        Right port -> (, Normal) $ connectionSrc pos mousePos isExp False (getPortNumber $ port ^. portId) numOfSameTypePorts $ countOutPorts node + countArgPorts node == 1
        Left  port -> (, Normal) $ connectionDst mousePos pos False isExp (getPortNumber $ port ^. portId) numOfSameTypePorts $ isSelf $ port ^. portId
    where
        pos                = node ^. position
        isExp              = not . isCollapsed $ node
        numOfSameTypePorts = case eport of
            Right _ -> countOutPorts node
            Left  _ -> countArgPorts node

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
