{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.ConnectionPen.ConnectionPen
    ( startConnecting
    , connectMove
    , stopConnecting
    ) where

import           Data.Curve                                  (CurveSegment, getPointsOnCurveSegment)
import qualified Data.Curve                                  as Curve
import           Data.Position                               (distance)
import           Data.Timestamp                              (Timestamp)
import           Luna.Studio.Action.Batch                    (autoconnect)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.ConnectionPen.SmoothLine (addPointToCurve, beginCurve, curveToSvgPath)
import           Luna.Studio.Action.Geometry.ConnectionPen   (getNodeAtPosition)
import           Luna.Studio.Data.Color                      (Color (Color))
import           Luna.Studio.Event.Mouse                     (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.ConnectionPen       (ConnectionPen (ConnectionPen))
import qualified Luna.Studio.React.Model.ConnectionPen       as ConnectionPen
import qualified Luna.Studio.React.Model.NodeEditor          as NodeEditor
import           Luna.Studio.State.Action                    (Action (begin, continue, end, update), PenConnect (PenConnect),
                                                              penConnectAction)
import qualified Luna.Studio.State.Action                    as Action
import           Luna.Studio.State.Global                    (State, beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                              updateActionWithKey)
import qualified Luna.Studio.State.Global                    as Global
import           React.Flux                                  (MouseEvent)


instance Action (Command State) PenConnect where
    begin    = beginActionWithKey    penConnectAction
    continue = continueActionWithKey penConnectAction
    update   = updateActionWithKey   penConnectAction
    end _    = removeActionFromState penConnectAction


startConnecting :: MouseEvent -> Timestamp -> Command State ()
startConnecting evt timestamp = do
    pos <- workspacePosition evt
    let curve = beginCurve pos timestamp
    begin $ PenConnect curve Nothing
    Global.modifyNodeEditor $ NodeEditor.connectionPen ?= ConnectionPen (curveToSvgPath curve) (Color 1)

connectProcessSegment :: CurveSegment -> PenConnect -> Command State ()
connectProcessSegment seg state = do
    let beg = seg ^. Curve.segmentBegin
        end = seg ^. Curve.segmentEnd
        numOfPoints = round $ distance beg end
        points = getPointsOnCurveSegment seg numOfPoints
    intersectedNodes <- catMaybes <$> mapM getNodeAtPosition (beg:points)
    when (not $ null intersectedNodes) $ do
        let uniqueIntersectedNodes = map head $ group intersectedNodes
        let nodesToConnect = case state ^. Action.penConnectLastVisitedNode of
                Just nodeId -> zip (nodeId : uniqueIntersectedNodes) uniqueIntersectedNodes
                Nothing     -> zip uniqueIntersectedNodes $ tail uniqueIntersectedNodes
        mapM_ (\(id1, id2) -> when (id1 /= id2) $ autoconnect id1 id2) nodesToConnect
        update $ state & Action.penConnectLastVisitedNode ?~ last uniqueIntersectedNodes

connectMove :: MouseEvent -> Timestamp -> PenConnect -> Command State ()
connectMove evt timestamp state = do
    pos <- workspacePosition evt
    let curve  = addPointToCurve pos timestamp $ state ^. Action.penConnectCurve
        state' = state & Action.penConnectCurve .~ curve
    update state'
    Global.modifyNodeEditor $ NodeEditor.connectionPen . _Just . ConnectionPen.path .= curveToSvgPath curve
    when ((length $ curve ^. Curve.segments) > 1 && (head $ curve ^. Curve.segments) ^. Curve.approved) $ do
        connectProcessSegment (head $ drop 1 $ curve ^. Curve.segments) state'

stopConnecting :: PenConnect -> Command State ()
stopConnecting state = do
    let curve = state ^. Action.penConnectCurve
    when (not $ (head $ curve ^. Curve.segments) ^. Curve.approved) $ do
        connectProcessSegment (head $ curve ^. Curve.segments) state
    removeActionFromState penConnectAction
    Global.modifyNodeEditor $ NodeEditor.connectionPen .= Nothing
