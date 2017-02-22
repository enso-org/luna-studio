module Luna.Studio.Action.Camera.Modify
     ( modifyCamera
     , resetCamera
     ) where

import           Data.Matrix                           (Matrix, identity, inverse, multStd2)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Action.Graph.Update       (updateConnectionsForEdges)
import           Luna.Studio.Data.CameraTransformation (lastInverse, logicalToScreen, screenToLogical)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global

--TODO[LJK]: connections to edges should be updated at the same time as camera
modifyCamera :: Matrix Double -> Matrix Double -> Command State ()
modifyCamera matrix invertedMatrix = do
    Global.modifyNodeEditor $ do
        NodeEditor.screenTransform . logicalToScreen %= flip multStd2 matrix
        transformsSinceLastInverse <- use $ NodeEditor.screenTransform . lastInverse
        if transformsSinceLastInverse < 100
        then do
            originalMatrix <- use $ NodeEditor.screenTransform . logicalToScreen
            case inverse originalMatrix of
                Right m -> do
                    NodeEditor.screenTransform . screenToLogical .= m
                    NodeEditor.screenTransform . lastInverse     .= 0
                _       -> do
                    NodeEditor.screenTransform . screenToLogical %= multStd2 invertedMatrix
                    NodeEditor.screenTransform . lastInverse     += 1
        else do
            NodeEditor.screenTransform . screenToLogical %= multStd2 invertedMatrix
            NodeEditor.screenTransform . lastInverse     += 1
    updateConnectionsForEdges

resetCamera :: Command State ()
resetCamera = do
    Global.modifyNodeEditor $ do
        NodeEditor.screenTransform . logicalToScreen .= identity 4
        NodeEditor.screenTransform . screenToLogical .= identity 4
    updateConnectionsForEdges
