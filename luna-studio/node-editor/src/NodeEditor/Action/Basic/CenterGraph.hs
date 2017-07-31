module NodeEditor.Action.Basic.CenterGraph where

import           Common.Action.Command                      (Command)
import           Common.Prelude                             hiding (span)
import           Data.Matrix                                (multStd2)
import           LunaStudio.Data.CameraTransformation       (lastInverse, logicalToScreen, screenToLogical)
import           LunaStudio.Data.Position                   (minimumRectangle, vector, x, y)
import           LunaStudio.Data.Size                       (Size (Size))
import           LunaStudio.Data.Vector2                    (Vector2 (Vector2), scalarProduct)
import           NodeEditor.Action.Basic.ModifyCamera       (resetCamera)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodes, modifyNodeEditor)
import           NodeEditor.Action.State.Scene              (getScreenCenter, getScreenSize)
import           NodeEditor.Data.Matrix                     (homothetyMatrix, invertedHomothetyMatrix, invertedTranslationMatrix,
                                                             translationMatrix)
import           NodeEditor.React.Model.Node.ExpressionNode (position)
import           NodeEditor.React.Model.NodeEditor          (screenTransform)
import           NodeEditor.State.Global                    (State)


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
