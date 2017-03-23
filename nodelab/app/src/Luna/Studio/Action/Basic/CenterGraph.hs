module Luna.Studio.Action.Basic.CenterGraph where

import           Data.Matrix                                 (multStd2)
import           Data.Position                               (minimumRectangle, vector, x, y)
import           Data.Size                                   (Size (Size))
import           Data.Vector                                 (Vector2 (Vector2), scalarProduct)
import           Luna.Studio.Action.Basic.DrawConnection     (redrawConnectionsForEdgeNodes)
import           Luna.Studio.Action.Basic.ModifyCamera       (resetCamera)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNodes, modifyNodeEditor)
import           Luna.Studio.Action.State.Scene              (getScreenCenter, getScreenSize)
import           Luna.Studio.Data.CameraTransformation       (lastInverse, logicalToScreen, screenToLogical)
import           Luna.Studio.Data.Matrix                     (homothetyMatrix, invertedHomothetyMatrix, invertedTranslationMatrix,
                                                              translationMatrix)
import           Luna.Studio.Prelude                         hiding (span)
import           Luna.Studio.React.Model.Node.ExpressionNode (position)
import           Luna.Studio.React.Model.NodeEditor          (screenTransform)
import           Luna.Studio.State.Global                    (State)


padding :: Vector2 Double
padding = Vector2 80 80

centerGraph :: Command State ()
centerGraph = do
    nodes <- getExpressionNodes
    case minimumRectangle $ map (view position) nodes of
        Just (leftTop, rightBottom) -> do
            mayScreenSize   <- getScreenSize
            mayScreenCenter <- getScreenCenter
            withJust ((,) <$> mayScreenSize <*> mayScreenCenter) $ \(screenSize, screenCenter) -> do
                let span         = Size (rightBottom ^. vector - leftTop ^. vector + scalarProduct padding 2)
                    shift        = padding + screenCenter ^. vector - scalarProduct (span ^. vector) 0.5 - leftTop ^. vector
                    factor       = min 1 $ min (screenSize ^. x / span ^. x) (screenSize ^. y / span ^. y)
                modifyNodeEditor $ do
                    screenTransform . logicalToScreen .= multStd2 (translationMatrix shift) (homothetyMatrix screenCenter factor)
                    screenTransform . screenToLogical .= multStd2 (invertedHomothetyMatrix screenCenter factor) (invertedTranslationMatrix shift)
                    screenTransform . lastInverse     .= 2
                void redrawConnectionsForEdgeNodes
        Nothing -> resetCamera
