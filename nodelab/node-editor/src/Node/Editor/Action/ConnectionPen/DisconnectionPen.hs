{-# OPTIONS_GHC -fno-warn-orphans #-}
module Node.Editor.Action.ConnectionPen.DisconnectionPen
    ( startDisconnecting
    , disconnectMove
    , stopDisconnecting
    ) where

import           Data.Curve                                  (CurveSegment, getPointsOnCurveSegment)
import qualified Data.Curve                                  as Curve
import qualified Data.HashMap.Strict                         as HashMap
import           Data.Position                               (Position, distance)
import           Data.Timestamp                              (Timestamp)
import           Node.Editor.Action.Basic.RemoveConnection   (removeConnection, removeConnectionsBetweenNodes)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.ConnectionPen.SmoothLine (addPointToCurve, beginCurve, curveToSvgPath)
import           Node.Editor.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                              updateActionWithKey)
import           Node.Editor.Action.State.Model              (getConnectionsIntersectingSegment, getNodeAtPosition)
import           Node.Editor.Action.State.NodeEditor         (getConnectionsMap, modifyNodeEditor)
import           Node.Editor.Data.Color                      (Color (Color))
import           Node.Editor.Event.Mouse                     (workspacePosition)
import           Luna.Prelude
import           Node.Editor.React.Model.Connection          (Connection, nodeLocs)
import           Node.Editor.React.Model.ConnectionPen       (ConnectionPen (ConnectionPen))
import qualified Node.Editor.React.Model.ConnectionPen       as ConnectionPen
import qualified Node.Editor.React.Model.NodeEditor          as NodeEditor
import           Node.Editor.State.Action                    (Action (begin, continue, end, update), PenDisconnect (PenDisconnect),
                                                              penDisconnectAction, penDisconnectCurve, penDisconnectLastVisitedNode,
                                                              penDisconnectNextNodeRestriction)
import           Node.Editor.State.Global                    (State)
import           React.Flux                                  (MouseEvent)


instance Action (Command State) PenDisconnect where
    begin    = beginActionWithKey    penDisconnectAction
    continue = continueActionWithKey penDisconnectAction
    update   = updateActionWithKey   penDisconnectAction
    end      = stopDisconnecting


startDisconnecting :: MouseEvent -> Timestamp -> Command State ()
startDisconnecting evt timestamp = do
    pos <- workspacePosition evt
    let curve = beginCurve pos timestamp
    begin $ PenDisconnect curve Nothing Nothing
    modifyNodeEditor $ NodeEditor.connectionPen ?= ConnectionPen (curveToSvgPath curve) (Color 2)

checkAndUpdateRestriction :: Connection -> Command State ()
checkAndUpdateRestriction conn = continue $ \state -> withJust (state ^. penDisconnectLastVisitedNode) $ \lastNode -> do
    let (n1, n2) = conn ^. nodeLocs
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
    modifyNodeEditor $ NodeEditor.connectionPen . _Just . ConnectionPen.path .= curveToSvgPath curve
    when (length (curve ^. Curve.segments) > 1 && head (curve ^. Curve.segments) ^. Curve.approved) $
        disconnectProcessSegment $ head $ drop 1 $ curve ^. Curve.segments

stopDisconnecting :: PenDisconnect -> Command State ()
stopDisconnecting state = do
    let curve = state ^. penDisconnectCurve . Curve.segments
    disconnectProcessSegment $ head curve
    removeActionFromState penDisconnectAction
    modifyNodeEditor $ NodeEditor.connectionPen .= Nothing
