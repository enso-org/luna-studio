module Luna.Studio.Action.Geometry.Node
    ( nodeToNodeAngle
    , getIntersectingConnections
    ) where

import           Control.Monad.Trans.Maybe             (MaybeT (MaybeT), runMaybeT)
import qualified Data.HashMap.Strict                   as HashMap
import           Data.Position                         (Position, distanceSquared, move, vector, x, y)
import           Data.Vector                           (dotV, scalarProduct)
import           Empire.API.Data.Connection            (ConnectionId)
import           Empire.API.Data.Node                  (NodeId)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.Geometry.Constants (nodeRadius)
import           Luna.Studio.Data.Angle                (Angle)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Connection    as Connection
import           Luna.Studio.React.Model.Node          (Node)
import qualified Luna.Studio.React.Model.Node          as Node
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global
import qualified Luna.Studio.State.Graph               as Graph


nodeToNodeAngle :: Position -> Position -> Angle
nodeToNodeAngle src dst =
    let srcX = src ^. x
        srcY = src ^. y
        dstX = dst ^. x
        dstY = dst ^. y
    in  if srcX < dstX
        then atan ((srcY - dstY) / (srcX - dstX))
        else atan ((srcY - dstY) / (srcX - dstX)) + pi

getIntersectingConnections :: Node -> Position -> Command State (Maybe ConnectionId)
getIntersectingConnections node mousePos = do
    let nodeId       = node ^. Node.nodeId
        posToCompare = if node ^. Node.mode == Node.Collapsed then node ^. Node.position else mousePos
    connIds             <- use $ Global.graph . Graph.connectionsMap . to HashMap.keys
    intersecingConnIds' <- forM connIds $ distSqFromMouseIfIntersect nodeId posToCompare mousePos
    let intersecingConnIds = catMaybes intersecingConnIds'
    return $ if null intersecingConnIds then Nothing else
        Just $ fst $ minimumBy (\(_, distSq1) (_, distSq2) -> compare distSq1 distSq2) intersecingConnIds

distSqFromMouseIfIntersect :: NodeId -> Position -> Position -> ConnectionId -> Command State (Maybe (ConnectionId, Double))
distSqFromMouseIfIntersect nodeId nodePos mousePos connId = do
    nodeConnsIds <- Graph.connectionIdsContainingNode nodeId <$> use Global.graph
    if elem connId nodeConnsIds then return Nothing
    else runMaybeT $ do
        conn <- MaybeT $ Global.getConnection connId
        let srcPos  = conn ^. Connection.from
            dstPos  = conn ^. Connection.to
            proj    = closestPointOnLine (srcPos, dstPos) nodePos
            u       = closestPointOnLineParam (srcPos, dstPos) nodePos
        if u < 0 || u > 1 || distanceSquared proj nodePos > nodeRadius ^ (2 :: Integer) then
            nothing
        else return $ (connId, distanceSquared proj mousePos)


-- | From Graphics.Gloss.Geometry.Line
-- | Given an infinite line which intersects `P1` and `P1`,
--      return the point on that line that is closest to `P3`
{-# INLINE closestPointOnLine #-}
closestPointOnLine :: (Position, Position) -> Position -> Position
closestPointOnLine line@(p1, p2) p3 = move shift p1 where
    segVec = p2 ^. vector - p1 ^. vector
    u      = closestPointOnLineParam line p3
    shift  = scalarProduct segVec u

-- | From Graphics.Gloss.Geometry.Line
-- | Given an infinite line which intersects P1 and P2,
--      let P4 be the point on the line that is closest to P3.
--
--      Return an indication of where on the line P4 is relative to P1 and P2.
--
-- @
--      if P4 == P1 then 0
--      if P4 == P2 then 1
--      if P4 is halfway between P1 and P2 then 0.5
-- @
--
-- @
--        |
--       P1
--        |
--     P4 +---- P3
--        |
--       P2
--        |
-- @
--
{-# INLINE closestPointOnLineParam #-}
closestPointOnLineParam :: (Position, Position) -> Position -> Double
closestPointOnLineParam (p1, p2) p3 = (v3 - v1) `dotV` (v2 - v1) / (v2 - v1) `dotV` (v2 - v1) where
    v1 = p1 ^. vector
    v2 = p2 ^. vector
    v3 = p3 ^. vector
