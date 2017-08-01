{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.Camera.Pan
     ( resetPan
     , stopPanDrag
     , panLeft
     , panRight
     , panUp
     , panDown
     , panCamera
     , startPanDrag
     , panDrag
     ) where

import           Common.Action.Command                (Command)
import           Common.Prelude
import           Data.Matrix                          (setElem)
import           LunaStudio.Data.CameraTransformation (logicalToScreen, screenToLogical)
import           LunaStudio.Data.Matrix               (invertedTranslationMatrix, translationMatrix)
import           LunaStudio.Data.ScreenPosition       (ScreenPosition, vector)
import           LunaStudio.Data.Vector2              (Vector2 (Vector2))
import           NodeEditor.Action.Basic              (modifyCamera)
import           NodeEditor.Action.State.Action       (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                       updateActionWithKey)
import           NodeEditor.Action.State.NodeEditor   (modifyNodeEditor)
import           NodeEditor.React.Model.NodeEditor    (screenTransform)
import           NodeEditor.State.Action              (Action (begin, continue, end, update), PanDrag (PanDrag), panDragAction,
                                                       panDragPreviousPos)
import           NodeEditor.State.Global              (State)


instance Action (Command State) PanDrag where
    begin    = beginActionWithKey    panDragAction
    continue = continueActionWithKey panDragAction
    update   = updateActionWithKey   panDragAction
    end _    = removeActionFromState panDragAction

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
startPanDrag pos = begin $ PanDrag pos

panDrag :: ScreenPosition -> PanDrag -> Command State ()
panDrag actPos action = do
    let prevPos = action ^. panDragPreviousPos
        delta   = actPos ^. vector - prevPos ^. vector
    update $ PanDrag actPos
    panCamera delta

resetPan :: Command State ()
resetPan = modifyNodeEditor $ do
    screenTransform . logicalToScreen %= (setElem 0 (4,1) . setElem 0 (4,2))
    screenTransform . screenToLogical %= (setElem 0 (4,1) . setElem 0 (4,2))

stopPanDrag :: PanDrag -> Command State ()
stopPanDrag _ = removeActionFromState panDragAction
