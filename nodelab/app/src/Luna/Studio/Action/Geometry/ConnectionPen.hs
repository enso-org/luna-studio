module Luna.Studio.Action.Geometry.ConnectionPen
    ( getConnectionsIntersectingSegment
    , getNodeAtPosition
    ) where

import           Control.Monad                         (filterM)
import           Data.Position                         (Position (Position), distanceSquared, x, y)
import           Data.Vector                           (Vector2 (Vector2))
import           Empire.API.Data.Connection            (ConnectionId)
import           Empire.API.Data.Node                  (NodeId)
import           Luna.Studio.Action.Camera.Screen      (translateToWorkspace)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.Geometry.Constants (nodeRadius)
import           Luna.Studio.Action.Graph.Lookup       (allConnectionModels, allNodes)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Connection    as Model
import           Luna.Studio.React.Model.Node          (Node)
import qualified Luna.Studio.React.Model.Node          as Model
import           Luna.Studio.State.Global              (State)

foreign import javascript safe "document.getElementById($1).getBoundingClientRect().left"   expandedNodeLeft   :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().top"    expandedNodeTop    :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().right"  expandedNodeRight  :: JSString -> IO Double
foreign import javascript safe "document.getElementById($1).getBoundingClientRect().bottom" expandedNodeBottom :: JSString -> IO Double

-- Line-Line intersection from Graphics.Gloss.Geometry.Line
-- | Given four points specifying two lines, get the point where the two lines
--   cross, if any. Note that the lines extend off to infinity, so the
--   intersection point might not line between either of the two pairs of points.
--
-- @
--     \\      /
--      P1  P4
--       \\ /
--        +
--       / \\
--      P3  P2
--     /     \\
-- @
intersectLineLine :: (Position, Position) -> (Position, Position) -> Maybe Position
intersectLineLine (p1, p2) (p3, p4) = do
    let x1   = p1 ^. x
        y1   = p1 ^. y
        x2   = p2 ^. x
        y2   = p2 ^. y
        x3   = p3 ^. x
        y3   = p3 ^. y
        x4   = p4 ^. x
        y4   = p4 ^. y
        dx12 = x1 - x2
        dx34 = x3 - x4
        dy12 = y1 - y2
        dy34 = y3 - y4
        den  = dx12 * dy34  - dy12 * dx34
    if den == 0 then
        Nothing
    else do
        let det12   = x1 * y2 - y1 * x2
            det34   = x3 * y4 - y3 * x4
            numx    = det12 * dx34 - dx12 * det34
            numy    = det12 * dy34 - dy12 * det34
        Just (Position (Vector2 (numx / den) (numy / den)))

doesSegmentsIntersects :: (Position, Position) -> (Position, Position) -> Command State Bool
doesSegmentsIntersects seg1@(beg1, end1) seg2@(beg2, end2) =
    case intersectLineLine seg1 seg2 of
        Nothing -> do
            let isBeg1OnSeg2 = distanceSquared beg1 beg2 + distanceSquared beg1 end2 == distanceSquared beg2 end2
                isBeg2OnSeg1 = distanceSquared beg2 beg1 + distanceSquared beg2 end1 == distanceSquared beg1 end1
            return $ isBeg1OnSeg2 || isBeg2OnSeg1
        Just p  -> do
            let leftTop1     = Position $ Vector2 (min (beg1 ^. x) (end1 ^. x)) (min (beg1 ^. y) (end1 ^. y))
                rightBottom1 = Position $ Vector2 (max (beg1 ^. x) (end1 ^. x)) (max (beg1 ^. y) (end1 ^. y))
                leftTop2     = Position $ Vector2 (min (beg2 ^. x) (end2 ^. x)) (min (beg2 ^. y) (end2 ^. y))
                rightBottom2 = Position $ Vector2 (max (beg2 ^. x) (end2 ^. x)) (max (beg2 ^. y) (end2 ^. y))
            return $ isPointInRect p (leftTop1, rightBottom1) && isPointInRect p (leftTop2, rightBottom2)

epsilon :: Double
epsilon = 0.005

isPointInRect :: Position -> (Position, Position) -> Bool
isPointInRect pos (leftTop, rightBottom) = pos ^. x >= leftTop ^. x     - epsilon
                                        && pos ^. x <= rightBottom ^. x + epsilon
                                        && pos ^. y >= leftTop ^. y     - epsilon
                                        && pos ^. y <= rightBottom ^. y + epsilon

isPointInCircle :: Position -> (Position, Double) -> Bool
isPointInCircle p (circleCenter, radius) = distanceSquared p circleCenter <= radius ^ (2 :: Integer)

isPointInNode :: Position -> Node -> Command State Bool
isPointInNode p node =
    if node ^. Model.isExpanded then do
        let nodeId = node ^. Model.nodeId
        left        <- liftIO $ expandedNodeLeft   $ fromString $ "node-" <> show nodeId
        right       <- liftIO $ expandedNodeRight  $ fromString $ "node-" <> show nodeId
        top         <- liftIO $ expandedNodeTop    $ fromString $ "node-" <> show nodeId
        bottom      <- liftIO $ expandedNodeBottom $ fromString $ "node-" <> show nodeId
        leftTop     <- translateToWorkspace $ Position (Vector2 left  top)
        rightBottom <- translateToWorkspace $ Position (Vector2 right bottom)
        return $ isPointInRect p (leftTop, rightBottom)
    else return $ isPointInCircle p ((node ^. Model.position), nodeRadius)

getNodeAtPosition :: Position -> Command State (Maybe NodeId)
getNodeAtPosition p = do
    nodes <- allNodes >>= filterM (isPointInNode p)
    if null nodes then
        return Nothing
    else return $ Just $ maximumBy (\node1 node2 -> compare (node1 ^. Model.zPos) (node2 ^. Model.zPos)) nodes ^. Model.nodeId

getConnectionsIntersectingSegment :: (Position, Position) -> Command State [ConnectionId]
getConnectionsIntersectingSegment seg = do
    connections <- allConnectionModels
    toRemove    <- filterM (\conn -> doesSegmentsIntersects seg (conn ^. Model.from, conn ^. Model.to)) connections
    return $ map (view Model.connectionId) toRemove
