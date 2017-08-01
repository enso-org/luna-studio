module NodeEditor.Action.Basic.ModifyCamera where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Data.Matrix                                (Matrix, inverse, multStd2)
import           LunaStudio.Data.CameraTransformation       (lastInverse, logicalToScreen, screenToLogical)
import           LunaStudio.Data.CameraTransformation       (getCameraForRectangle)
import           LunaStudio.Data.Position                   (minimumRectangle)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodes, modifyNodeEditor, setScreenTransform)
import           NodeEditor.Action.State.Scene              (getScreenSize)
import           NodeEditor.React.Model.Node.ExpressionNode (position)
import           NodeEditor.React.Model.NodeEditor          (screenTransform)
import           NodeEditor.State.Global                    (State)


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
resetCamera = setScreenTransform def


centerGraph :: Command State ()
centerGraph = do
    nodes         <- getExpressionNodes
    mayScreenSize <- getScreenSize
    let camera = maybe def (flip getCameraForRectangle mayScreenSize) $ minimumRectangle $ map (view position) nodes
    setScreenTransform camera
