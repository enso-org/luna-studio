{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.ConnectionPen.ConnectionPen
    ( startConnecting
    , connectMove
    , stopConnecting
    ) where

import           Common.Prelude
import           Data.Curve                                 (CurveSegment, getPointsOnCurveSegment)
import qualified Data.Curve                                 as Curve
import           Data.Timestamp                             (Timestamp)
import           Empire.API.Data.Position                   (distance)
import           NodeEditor.Action.Basic                    (connect, updateAllPortsSelfVisibility)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.ConnectionPen.SmoothLine (addPointToCurve, beginCurve, curveToSvgPath)
import           NodeEditor.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                             updateActionWithKey)
import           NodeEditor.Action.State.Model              (getNodeAtPosition)
import           NodeEditor.Action.State.NodeEditor         (modifyNodeEditor)
import           NodeEditor.Data.Color                      (Color (Color))
import           NodeEditor.Event.Mouse                     (workspacePosition)
import           NodeEditor.React.Model.ConnectionPen       (ConnectionPen (ConnectionPen))
import qualified NodeEditor.React.Model.ConnectionPen       as ConnectionPen
import qualified NodeEditor.React.Model.NodeEditor          as NodeEditor
import           NodeEditor.State.Action                    (Action (begin, continue, end, update), PenConnect (PenConnect),
                                                             penConnectAction, penConnectCurve, penConnectLastVisitedNode)
import           NodeEditor.State.Global                    (State)
import           React.Flux                                 (MouseEvent)


instance Action (Command State) PenConnect where
    begin    = beginActionWithKey    penConnectAction
    continue = continueActionWithKey penConnectAction
    update   = updateActionWithKey   penConnectAction
    end      = stopConnecting


startConnecting :: MouseEvent -> Timestamp -> Command State ()
startConnecting evt timestamp = do
    pos <- workspacePosition evt
    let curve = beginCurve pos timestamp
    begin $ PenConnect curve Nothing
    updateAllPortsSelfVisibility
    modifyNodeEditor $ NodeEditor.connectionPen ?= ConnectionPen (curveToSvgPath curve) (Color 1)

connectProcessSegment :: CurveSegment -> PenConnect -> Command State ()
connectProcessSegment seg state = do
    let segBeg = seg ^. Curve.segmentBegin
        segEnd = seg ^. Curve.segmentEnd
        numOfPoints = round $ distance segBeg segEnd
        points = getPointsOnCurveSegment seg numOfPoints
    intersectedNodes <- catMaybes <$> mapM getNodeAtPosition (segBeg:points)
    unless (null intersectedNodes) $ do
        let uniqueIntersectedNodes = map head $ group intersectedNodes
        let nodesToConnect = case state ^. penConnectLastVisitedNode of
                Just nodeLoc -> zip (nodeLoc : uniqueIntersectedNodes) uniqueIntersectedNodes
                Nothing      -> zip uniqueIntersectedNodes $ tail uniqueIntersectedNodes
        mapM_ (\(id1, id2) -> when (id1 /= id2) $ connect (Right id1) (Right id2)) nodesToConnect
        update $ state & penConnectLastVisitedNode ?~ last uniqueIntersectedNodes

connectMove :: MouseEvent -> Timestamp -> PenConnect -> Command State ()
connectMove evt timestamp state = do
    pos <- workspacePosition evt
    let curve  = addPointToCurve pos timestamp $ state ^. penConnectCurve
        state' = state & penConnectCurve .~ curve
    update state'
    modifyNodeEditor $ NodeEditor.connectionPen . _Just . ConnectionPen.path .= curveToSvgPath curve
    when (length (curve ^. Curve.segments) > 1 && head (curve ^. Curve.segments) ^. Curve.approved) $
        connectProcessSegment (head $ drop 1 $ curve ^. Curve.segments) state'

stopConnecting :: PenConnect -> Command State ()
stopConnecting state = do
    let curve = state ^. penConnectCurve
    unless ((head $ curve ^. Curve.segments) ^. Curve.approved) $
        connectProcessSegment (head $ curve ^. Curve.segments) state
    modifyNodeEditor $ NodeEditor.connectionPen .= Nothing
    updateAllPortsSelfVisibility
    removeActionFromState penConnectAction
