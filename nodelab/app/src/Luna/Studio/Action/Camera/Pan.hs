{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.Camera.Pan
     ( resetPan
     , resetPanState
     , panLeft
     , panRight
     , panUp
     , panDown
     , panCamera
     , startPanDrag
     , panDrag
     ) where

import           Data.Matrix                           (setElem)
import           Luna.Studio.Action.Camera.Modify      (modifyCamera)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Data.CameraTransformation (logicalToScreen, screenToLogical)
import           Luna.Studio.Data.Matrix               (invertedTranslationMatrix, translationMatrix)
import           Luna.Studio.Data.Vector               (ScreenPosition, Vector2 (Vector2), vector)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.State.Action              (Action (PanDrag))
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global
import qualified Luna.Studio.State.PanDrag             as PanDrag
import           Luna.Studio.State.StatefulAction      (StatefulAction (continue, exit, matchState, pack, start, update))


instance StatefulAction PanDrag.State where
    matchState (PanDrag state) = Just state
    matchState _ = Nothing
    pack = PanDrag
    exit = resetPanState

panStep :: Double
panStep = 50

panCamera :: Vector2 Double -> Command State ()
panCamera delta = modifyCamera (translationMatrix delta) (invertedTranslationMatrix delta)

panLeft, panRight, panUp, panDown :: Command State ()
panLeft  = panCamera $ Vector2 (-panStep) 0
panRight = panCamera $ Vector2 panStep    0
panUp    = panCamera $ Vector2 0          (-panStep)
panDown  = panCamera $ Vector2 0          panStep

startPanDrag :: ScreenPosition -> Command State ()
startPanDrag pos = start $ PanDrag.State pos

panDrag :: ScreenPosition -> PanDrag.State -> Command State ()
panDrag actPos state = do
    let prevPos = view PanDrag.previousPos state
        delta = actPos ^. vector - prevPos ^. vector
    update $ PanDrag.State actPos
    panCamera delta

resetPan :: Command State ()
resetPan = Global.modifyNodeEditor $ do
    NodeEditor.screenTransform . logicalToScreen %= (setElem 0 (4,1) . setElem 0 (4,2))
    NodeEditor.screenTransform . screenToLogical %= (setElem 0 (4,1) . setElem 0 (4,2))

resetPanState :: PanDrag.State -> Command State ()
resetPanState _ = Global.performedAction .= Nothing
