module Luna.Studio.Action.Basic.ModifyCamera where

import           Data.Matrix                             (Matrix, identity, inverse, multStd2)
import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForSidebarNodes)
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.State.NodeEditor     (modifyNodeEditor)
import           Luna.Studio.Data.CameraTransformation   (lastInverse, logicalToScreen, screenToLogical)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.NodeEditor      (screenTransform)
import           Luna.Studio.State.Global                (State)


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
    void $ redrawConnectionsForSidebarNodes

resetCamera :: Command State ()
resetCamera = do
    modifyNodeEditor $ do
        screenTransform . logicalToScreen .= identity 4
        screenTransform . screenToLogical .= identity 4
    void $ redrawConnectionsForSidebarNodes
