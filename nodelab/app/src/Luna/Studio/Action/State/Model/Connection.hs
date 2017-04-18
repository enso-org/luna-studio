{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.State.Model.Connection where

import           Control.Monad.Trans.Maybe                   (MaybeT (MaybeT), runMaybeT)
import           Data.Position                               (Position, distanceSquared)
import           Empire.API.Data.PortRef                     (AnyPortRef (OutPortRef'), InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef                     as PortRef
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getConnection, getConnections, getLayout, getNode, getPort)
import           Luna.Studio.Data.Color                      (Color (Color))
import           Luna.Studio.Data.Geometry                   (closestPointOnLine, closestPointOnLineParam, doesSegmentsIntersects)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection          (Connection (Connection), ConnectionId, CurrentConnection (CurrentConnection),
                                                              Mode (Normal), connectionId, connectionPositionAndMode, containsNode,
                                                              currentConnectionSrcPositionAndMode, portPhantomPosition, toConnection)
import qualified Luna.Studio.React.Model.Connection          as Model
import           Luna.Studio.React.Model.Constants           (nodeRadius)
import           Luna.Studio.React.Model.Node                (Node (Expression), NodeLoc, hasPort)
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, countArgPorts, isCollapsed, nodeLoc, position)
import           Luna.Studio.React.Model.Port                (color, getPortNumber)
import           Luna.Studio.State.Global                    (State)


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
            createCurrentConnectionModel (OutPortRef' srcPortRef) $ portPhantomPosition n
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
            Expression n -> return (portPhantomPosition n, Normal, Color 0)
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
