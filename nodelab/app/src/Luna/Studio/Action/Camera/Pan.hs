module Luna.Studio.Action.Camera.Pan
     ( resetPan
     , panLeft
     , panRight
     , panUp
     , panDown
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
import qualified Luna.Studio.State.Camera              as Camera
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global


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
startPanDrag pos = Global.cameraState ?= Camera.PanDrag pos

panDrag :: ScreenPosition -> Command State ()
panDrag actPos = do
    mayState <- use Global.cameraState
    withJust mayState $ \state -> case state of
        Camera.PanDrag prevPos -> do
            Global.cameraState ?= Camera.PanDrag actPos
            let delta = actPos ^. vector - prevPos ^. vector
            panCamera delta
        _ -> return ()

resetPan :: Command State ()
resetPan = Global.modifyNodeEditor $ do
    NodeEditor.screenTransform . logicalToScreen %= (setElem 0 (4,1) . setElem 0 (4,2))
    NodeEditor.screenTransform . screenToLogical %= (setElem 0 (4,1) . setElem 0 (4,2))
