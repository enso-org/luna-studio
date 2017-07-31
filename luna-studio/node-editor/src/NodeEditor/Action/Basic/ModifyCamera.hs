module NodeEditor.Action.Basic.ModifyCamera where

import           Common.Action.Command                (Command)
import           Common.Prelude
import           Data.Matrix                          (Matrix, identity, inverse, multStd2)
import           LunaStudio.Data.CameraTransformation (lastInverse, logicalToScreen, screenToLogical)
import           NodeEditor.Action.State.NodeEditor   (modifyNodeEditor)
import           NodeEditor.React.Model.NodeEditor    (screenTransform)
import           NodeEditor.State.Global              (State)


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
