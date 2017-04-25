{-# LANGUAGE MultiWayIf #-}
module NodeEditor.Action.State.Model.Connection where

import           Common.Prelude
import           Control.Monad.Trans.Maybe                  (MaybeT (MaybeT), runMaybeT)
import           Data.Position                              (Position, distanceSquared)
import           Empire.API.Data.PortRef                    (AnyPortRef, InPortRef, OutPortRef)
import qualified Empire.API.Data.PortRef                    as PortRef
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.State.NodeEditor         (getConnection, getNode, getNodeEditor, getPosConnection, getPosConnections)
import           NodeEditor.Data.Geometry                   (closestPointOnLine, closestPointOnLineParam, doesSegmentsIntersects)
import           NodeEditor.React.Model.Connection          (Connection (Connection), ConnectionId, HalfConnection (HalfConnection),
                                                             connectionId, connectionMode, containsNode, halfConnectionMode)
import qualified NodeEditor.React.Model.Connection          as Model
import           NodeEditor.React.Model.Constants           (nodeRadius)
import           NodeEditor.React.Model.Node                (NodeLoc)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, isCollapsed, nodeLoc, position)
import           NodeEditor.React.Model.NodeEditor          (toPosConnection)
import           NodeEditor.State.Global                    (State)


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
        if srcPos == dstPos || u < 0 || u > 1 || distSq > nodeRadius ^ (2 :: Integer)
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
