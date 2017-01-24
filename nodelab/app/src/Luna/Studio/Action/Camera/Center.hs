module Luna.Studio.Action.Camera.Center
     ( centerGraph
     ) where

import           Data.Matrix                           (multStd2)
import           Data.Position                         (Position (Position), Vector2 (Vector2), fromTuple, minimumRectangle, scalarProduct,
                                                        vector, x, y)
import           Data.Size                             (Size (Size))
import qualified Empire.API.Data.Node                  as Node
import           Luna.Studio.Action.Camera.Modify      (resetCamera)
import           Luna.Studio.Action.Camera.Screen      (getScreenCenterFromSize, getScreenSize)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Data.CameraTransformation (lastInverse, logicalToScreen, screenToLogical)
import           Luna.Studio.Data.Matrix               (homothetyMatrix, invertedHomothetyMatrix, invertedTranslationMatrix,
                                                        translationMatrix)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global
import qualified Luna.Studio.State.Graph               as Graph


padding :: Vector2 Double
padding = Vector2 80 80

centerGraph :: Command State ()
centerGraph = do
    nodes <- use $ Global.graph . Graph.nodes
    case minimumRectangle $ map (Position . fromTuple) $ view Node.position <$> nodes of
        Just (leftTop, rightBottom) -> do
            screenSize <- getScreenSize
            let screenCenter = getScreenCenterFromSize screenSize
                span         = Size (rightBottom ^. vector - leftTop ^. vector + scalarProduct padding 2)
                shift        = padding + screenCenter ^. vector - scalarProduct (span ^. vector) 0.5 - leftTop ^. vector
                factor       = min 1 $ min (screenSize ^. x / span ^. x) (screenSize ^. y / span ^. y)

            Global.modifyNodeEditor $ do
                NodeEditor.screenTransform . logicalToScreen .= multStd2 (translationMatrix shift) (homothetyMatrix screenCenter factor)
                NodeEditor.screenTransform . screenToLogical .= multStd2 (invertedHomothetyMatrix screenCenter factor) (invertedTranslationMatrix shift)
                NodeEditor.screenTransform . lastInverse     .= 2
        Nothing -> resetCamera
