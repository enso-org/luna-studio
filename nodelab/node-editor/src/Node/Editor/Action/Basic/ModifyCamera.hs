module Node.Editor.Action.Basic.ModifyCamera where

import           Data.Matrix                           (Matrix, identity, inverse, multStd2)
import           Node.Editor.Action.Command            (Command)
import           Node.Editor.Action.State.NodeEditor   (modifyNodeEditor)
import           Node.Editor.Data.CameraTransformation (lastInverse, logicalToScreen, screenToLogical)
import           Luna.Prelude
import           Node.Editor.React.Model.NodeEditor    (screenTransform)
import           Node.Editor.State.Global              (State)


modifyCamera :: Matrix Double -> Matrix Double -> Command State ()
modifyCamera matrix invertedMatrix = do
    modifyNodeEditor $ do
        screenTransform . logicalToScreen %= flip multStd2 matrix
        transformsSinceLastInverse <- use $ screenTransform . lastInverse
        if transformsSinceLastInverse < 100
            then do
                originalMatrix <- use $ screenTransform . logicalToScreen
                case inverse originalMatrix of
                    Right m -> do
                        screenTransform . screenToLogical .= m
                        screenTransform . lastInverse     .= 0
                    _       -> do
                        screenTransform . screenToLogical %= multStd2 invertedMatrix
                        screenTransform . lastInverse     += 1
            else do
                screenTransform . screenToLogical %= multStd2 invertedMatrix
                screenTransform . lastInverse     += 1

resetCamera :: Command State ()
resetCamera = modifyNodeEditor $ do
    screenTransform . logicalToScreen .= identity 4
    screenTransform . screenToLogical .= identity 4
