module Luna.Studio.Action.Camera.Screen
     ( getScreenCenter
     , getScreenCenterFromSize
     , getScreenSize
     , getWorkspacePos
     , translateToWorkspace
     ) where

import           Data.Matrix                           (getElem, multStd2)
import qualified Data.Matrix                           as Matrix
import           Data.Position                         (Position (Position), ScreenPosition, Vector2 (Vector2))
import           Data.Size                             (Size (Size))
import           Data.Vector                           (fromTuple, scalarProduct, vector, x, y)
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Data.CameraTransformation (screenToLogical)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global



foreign import javascript safe "document.getElementById('Graph').offsetWidth"  workspaceWidth  :: IO Double
foreign import javascript safe "document.getElementById('Graph').offsetHeight" workspaceHeight :: IO Double
foreign import javascript safe "document.getElementById('Graph').getBoundingClientRect().left" workspaceLeft :: IO Double
foreign import javascript safe "document.getElementById('Graph').getBoundingClientRect().top"  workspaceTop  :: IO Double

getWorkspacePos :: MonadIO m => m Position
getWorkspacePos = Position .: Vector2 <$> liftIO workspaceLeft <*> liftIO workspaceTop

getScreenCenter :: MonadIO m => m ScreenPosition
getScreenCenter = getScreenCenterFromSize <$> getScreenSize

getScreenCenterFromSize :: Size -> ScreenPosition
getScreenCenterFromSize = Position . flip scalarProduct 0.5 . view vector

getScreenSize :: MonadIO m => m Size
getScreenSize = liftIO $ Size . fromTuple <$> ((,) <$> workspaceWidth <*> workspaceHeight)

translateToWorkspace :: ScreenPosition -> Command State Position
translateToWorkspace pos = do
    transformMatrix <- view (NodeEditor.screenTransform . screenToLogical) <$> Global.getNodeEditor
    let posMatrix      = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace = multStd2 posMatrix transformMatrix
    return $ Position $ fromTuple (getElem 1 1 posInWorkspace, getElem 1 2 posInWorkspace)
