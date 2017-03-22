{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.ConnectionPen.DisconnectionPen
    ( startDisconnecting
    , disconnectMove
    , stopDisconnecting
    ) where

import           Data.Curve                                  (CurveSegment, getPointsOnCurveSegment)
import qualified Data.Curve                                  as Curve
import qualified Data.HashMap.Strict                         as HashMap
import           Data.Position                               (Position, distance)
import           Data.Timestamp                              (Timestamp)
import           Luna.Studio.Action.Basic.RemoveConnection   (removeConnection, removeConnectionsBetweenNodes)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.ConnectionPen.SmoothLine (addPointToCurve, beginCurve, curveToSvgPath)
import           Luna.Studio.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                              updateActionWithKey)
import           Luna.Studio.Action.State.Model              (getConnectionsIntersectingSegment, getNodeAtPosition)
import           Luna.Studio.Action.State.NodeEditor         (getConnectionsMap, modifyExpressionNodeEditor)
import           Luna.Studio.Data.Color                      (Color (Color))
import           Luna.Studio.Event.Mouse                     (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection          (Connection, nodeIds)
import           Luna.Studio.React.Model.ConnectionPen       (ConnectionPen (ConnectionPen))
import qualified Luna.Studio.React.Model.ConnectionPen       as ConnectionPen
import qualified Luna.Studio.React.Model.NodeEditor          as NodeEditor
import           Luna.Studio.State.Action                    (Action (begin, continue, end, update), PenDisconnect (PenDisconnect),
                                                              penDisconnectAction, penDisconnectCurve, penDisconnectLastVisitedNode,
                                                              penDisconnectNextNodeRestriction)
import           Luna.Studio.State.Global                    (State)
import           React.Flux                                  (MouseEvent)


instance Action (Command State) PenDisconnect where
    begin    = beginActionWithKey    penDisconnectAction
    continue = continueActionWithKey penDisconnectAction
    update   = updateActionWithKey   penDisconnectAction
    end _    = removeActionFromState penDisconnectAction


startDisconnecting :: MouseEvent -> Timestamp -> Command State ()
startDisconnecting evt timestamp = do
    pos <- workspacePosition evt
    let curve = beginCurve pos timestamp
    begin $ PenDisconnect curve Nothing Nothing
    modifyExpressionNodeEditor $ NodeEditor.connectionPen ?= ConnectionPen (curveToSvgPath curve) (Color 2)

checkAndUpdateRestriction :: Connection -> Command State ()
checkAndUpdateRestriction conn = continue $ \state -> withJust (state ^. penDisconnectLastVisitedNode) $ \lastNode -> do
    let (n1, n2) = conn ^. nodeIds
        lastNodeInConn = lastNode == n1 || lastNode == n2
        newRestriction = if lastNode == n1 then n2 else n1
        restrictionPossible = case state ^. penDisconnectNextNodeRestriction of
            Just nodeId -> newRestriction == nodeId
            Nothing     -> True

    if lastNodeInConn && restrictionPossible then
        update $ state & penDisconnectNextNodeRestriction ?~ newRestriction
    else
        update $ state & penDisconnectNextNodeRestriction .~ Nothing
                       & penDisconnectLastVisitedNode     .~ Nothing

handleSegment :: (Position, Position) -> Command State ()
handleSegment seg@(_, segEnd) = do
    connectionIdsToRemove <- getConnectionsIntersectingSegment seg
    mapM_ removeConnection connectionIdsToRemove
    allConnections <- getConnectionsMap
    let connectionsToRemove = catMaybes $ flip map connectionIdsToRemove $ flip HashMap.lookup allConnections
    mapM_ checkAndUpdateRestriction connectionsToRemove

    mayNodeId <- getNodeAtPosition segEnd
    withJust mayNodeId $ \nodeId -> continue $ \action -> do
        let mayLastNodeId  = action ^. penDisconnectLastVisitedNode
            mayRestriction = action ^. penDisconnectNextNodeRestriction
        withJust mayLastNodeId $ \lastNodeId -> when (lastNodeId /= nodeId) $ do
            let meetsRestriction = maybe True (nodeId ==) mayRestriction
            when meetsRestriction $ removeConnectionsBetweenNodes nodeId lastNodeId
        update $ action & penDisconnectLastVisitedNode     ?~ nodeId
                        & penDisconnectNextNodeRestriction .~ Nothing

disconnectProcessSegment :: CurveSegment -> Command State ()
disconnectProcessSegment seg = do
    let segBeg = seg ^. Curve.segmentBegin
        segEnd = seg ^. Curve.segmentEnd
        numOfPoints = round $ distance segBeg segEnd
        points = getPointsOnCurveSegment seg numOfPoints
    mapM_ handleSegment $ zip (segBeg:points) (points ++ [segEnd])

disconnectMove :: MouseEvent -> Timestamp -> PenDisconnect -> Command State ()
disconnectMove evt timestamp state = do
    pos <- workspacePosition evt
    let curve = addPointToCurve pos timestamp $ state ^. penDisconnectCurve
        state'   = state & penDisconnectCurve .~ curve
    update state'
    modifyExpressionNodeEditor $ NodeEditor.connectionPen . _Just . ConnectionPen.path .= curveToSvgPath curve
    when (length (curve ^. Curve.segments) > 1 && head (curve ^. Curve.segments) ^. Curve.approved) $
        disconnectProcessSegment $ head $ drop 1 $ curve ^. Curve.segments

stopDisconnecting :: PenDisconnect -> Command State ()
stopDisconnecting state = do
    let curve = state ^. penDisconnectCurve . Curve.segments
    disconnectProcessSegment $ head curve
    removeActionFromState penDisconnectAction
    modifyExpressionNodeEditor $ NodeEditor.connectionPen .= Nothing
