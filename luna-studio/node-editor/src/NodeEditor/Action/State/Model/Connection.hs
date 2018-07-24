{-# LANGUAGE MultiWayIf #-}
module NodeEditor.Action.State.Model.Connection where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Control.Arrow                              ((&&&))
import           Control.Monad.Trans.Maybe                  (MaybeT (MaybeT), runMaybeT)
import           LunaStudio.Data.Geometry                   (closestPointOnLine, closestPointOnLineParam, doSegmentsIntersect)
import           LunaStudio.Data.PortRef                    (AnyPortRef, InPortRef, OutPortRef)
import qualified LunaStudio.Data.PortRef                    as PortRef
import           LunaStudio.Data.Position                   (Position, distanceSquared)
import           NodeEditor.Action.State.NodeEditor         (getConnection, getNode, getNodeEditor, getPosConnection, getPosConnections)
import           NodeEditor.React.Model.Connection          (Connection, ConnectionId, HalfConnection, connectionId, containsNode,
                                                             toConnection, toHalfConnection)
import qualified NodeEditor.React.Model.Connection          as Model
import           NodeEditor.React.Model.Constants           (nodeRadius)
import           NodeEditor.React.Model.Node                (NodeLoc)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, isCollapsed, nodeLoc, position)
import           NodeEditor.React.Model.NodeEditor          (toPosConnection)
import           NodeEditor.State.Global                    (State)


createConnectionModel :: OutPortRef -> InPortRef
    -> Command State (Maybe Connection)
createConnectionModel srcPortRef dstPortRef = runMaybeT $ do
    srcNode <- MaybeT $ getNode $ srcPortRef ^. PortRef.srcNodeLoc
    dstNode <- MaybeT $ getNode $ dstPortRef ^. PortRef.dstNodeLoc
    return $ toConnection srcPortRef dstPortRef srcNode dstNode

createHalfConnectionModel' :: OutPortRef -> InPortRef
    -> Command State (Maybe HalfConnection)
createHalfConnectionModel' outPortRef inPortRef = runMaybeT $ do
    conn <- MaybeT $ createConnectionModel outPortRef inPortRef
    ne <- lift getNodeEditor
    MaybeT $ return $ convert <$> toPosConnection ne conn

createHalfConnectionModel :: AnyPortRef -> Position
    -> Command State (Maybe HalfConnection)
createHalfConnectionModel anyPortRef dstPos = runMaybeT $ do
    srcNode <- MaybeT $ getNode $ anyPortRef ^. nodeLoc
    return $ toHalfConnection anyPortRef srcNode dstPos

distSqFromMouseIfIntersect :: NodeLoc -> Position -> ConnectionId
    -> Command State (Maybe (ConnectionId, Double))
distSqFromMouseIfIntersect nl nodePos connId = runMaybeT $ do
    conn  <- MaybeT $ getConnection  connId
    conn' <- MaybeT $ getPosConnection connId
    if containsNode nl conn then nothing else do
        let srcPos     = conn' ^. Model.srcPos
            dstPos     = conn' ^. Model.dstPos
            proj       = closestPointOnLine (srcPos, dstPos) nodePos
            u          = closestPointOnLineParam (srcPos, dstPos) nodePos
            distSq     = distanceSquared proj nodePos
            intersection
                = srcPos == dstPos
                || u < 0
                || u > 1
                || distSq > nodeRadius ^ (2 :: Integer)
        if intersection then nothing else return (connId, distSq)

getIntersectingConnections :: ExpressionNode -> Position
    -> Command State (Maybe ConnectionId)
getIntersectingConnections node mousePos = do
    let nl           = node ^. nodeLoc
        posToCompare = if isCollapsed node then node ^. position else mousePos
    connIds             <- map (view connectionId) <$> getPosConnections
    intersecingConnIds'
        <- forM connIds $ distSqFromMouseIfIntersect nl posToCompare
    let intersecingConnIds = catMaybes intersecingConnIds'
    return $ if null intersecingConnIds
        then Nothing
        else Just . fst $ minimumBy
            (\(_, distSq1) (_, distSq2) -> compare distSq1 distSq2)
            intersecingConnIds

getConnectionsIntersectingSegment :: (Position, Position)
    -> Command State [ConnectionId]
getConnectionsIntersectingSegment seg = fmap (view connectionId) . filter
    (doSegmentsIntersect seg . (view Model.srcPos &&& view Model.dstPos))
    <$> getPosConnections
