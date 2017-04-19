{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.State.Model.Connection where

import           Control.Monad.Trans.Maybe                   (MaybeT (MaybeT), runMaybeT)
import           Data.Position                               (Position, distanceSquared)
import           Empire.API.Data.PortRef                     (AnyPortRef, InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef                     as PortRef
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getConnection, getNode, getNodeEditor, getPosConnection, getPosConnections)
import           Luna.Studio.Data.Geometry                   (closestPointOnLine, closestPointOnLineParam, doesSegmentsIntersects)
import           Luna.Prelude
import           Luna.Studio.React.Model.Connection          (Connection (Connection), ConnectionId, HalfConnection (HalfConnection),
                                                              connectionId, connectionMode, containsNode, halfConnectionMode)
import qualified Luna.Studio.React.Model.Connection          as Model
import           Luna.Studio.React.Model.Constants           (nodeRadius)
import           Luna.Studio.React.Model.Node                (NodeLoc)
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, isCollapsed, nodeLoc, position)
import           Luna.Studio.React.Model.NodeEditor          (toPosConnection)
import           Luna.Studio.State.Global                    (State)


createConnectionModel :: OutPortRef -> InPortRef -> Command State (Maybe Connection)
createConnectionModel srcPortRef dstPortRef = runMaybeT $ do
    srcNode <- MaybeT $ getNode $ srcPortRef ^. PortRef.srcNodeLoc
    dstNode <- MaybeT $ getNode $ dstPortRef ^. PortRef.dstNodeLoc
    let mode = connectionMode srcNode dstNode
    return $ Connection srcPortRef dstPortRef mode


createHalfConnectionModel' :: OutPortRef -> InPortRef -> Command State (Maybe HalfConnection)
createHalfConnectionModel' outPortRef inPortRef = runMaybeT $ do
    conn <- MaybeT $ createConnectionModel outPortRef inPortRef
    ne <- lift getNodeEditor
    MaybeT $ return $ convert <$> toPosConnection ne conn

createHalfConnectionModel :: AnyPortRef -> Position -> Command State (Maybe HalfConnection)
createHalfConnectionModel anyPortRef dstPos = runMaybeT $ do
    srcNode <- MaybeT $ getNode $ anyPortRef ^. nodeLoc
    let mode = halfConnectionMode srcNode
    return $ HalfConnection anyPortRef dstPos mode

distSqFromMouseIfIntersect :: NodeLoc -> Position -> ConnectionId -> Command State (Maybe (ConnectionId, Double))
distSqFromMouseIfIntersect nl nodePos connId = runMaybeT $ do
    conn  <- MaybeT $ getConnection  connId
    conn' <- MaybeT $ getPosConnection connId
    if containsNode nl conn then nothing else do
        let srcPos  = conn' ^. Model.srcPos
            dstPos  = conn' ^. Model.dstPos
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
    connIds             <- map (view connectionId) <$> getPosConnections
    intersecingConnIds' <- forM connIds $ distSqFromMouseIfIntersect nl posToCompare
    let intersecingConnIds = catMaybes intersecingConnIds'
    return $ if null intersecingConnIds then Nothing else
        Just $ fst $ minimumBy (\(_, distSq1) (_, distSq2) -> compare distSq1 distSq2) intersecingConnIds

getConnectionsIntersectingSegment :: (Position, Position) -> Command State [ConnectionId]
getConnectionsIntersectingSegment seg = flip fmap getPosConnections $
    map (view connectionId) . filter (
        \conn -> doesSegmentsIntersects seg (conn ^. Model.srcPos, conn ^. Model.dstPos) )
