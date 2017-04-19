module Node.Editor.Action.Basic.CenterGraph where

import           Data.Matrix                                 (multStd2)
import           Data.Position                               (minimumRectangle, vector, x, y)
import           Data.Size                                   (Size (Size))
import           Data.Vector2                                (Vector2 (Vector2), scalarProduct)
import           Node.Editor.Action.Basic.ModifyCamera       (resetCamera)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.State.NodeEditor         (getExpressionNodes, modifyNodeEditor)
import           Node.Editor.Action.State.Scene              (getScreenCenter, getScreenSize)
import           Node.Editor.Data.CameraTransformation       (lastInverse, logicalToScreen, screenToLogical)
import           Node.Editor.Data.Matrix                     (homothetyMatrix, invertedHomothetyMatrix, invertedTranslationMatrix,
                                                              translationMatrix)
import           Luna.Prelude                         hiding (span)
import           Node.Editor.React.Model.Node.ExpressionNode (position)
import           Node.Editor.React.Model.NodeEditor          (screenTransform)
import           Node.Editor.State.Global                    (State)


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
        Nothing -> resetCamera
