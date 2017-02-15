module Luna.Studio.Action.Camera.Screen
     ( getScreenCenter
     , getScreenCenterFromSize
     , getScreenSize
     , getWorkspacePos
     , translateToWorkspace
     , getScreenLeftCenter
     , getScreenRightCenter
     ) where

import           Data.Matrix                           (getElem, multStd2)
import qualified Data.Matrix                           as Matrix
import           Data.Position                         (Position (Position), ScreenPosition)
import           Data.Size                             (Size)
import           Data.Vector                           (Vector2 (Vector2), fromTuple, scalarProduct, vector, x, y)
import qualified JS.Scene                              as Scene
import           Luna.Studio.Action.Command            (Command)
import           Luna.Studio.Data.CameraTransformation (screenToLogical)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.State.Global              (State)
import qualified Luna.Studio.State.Global              as Global



getWorkspacePos :: Command State Position
getWorkspacePos = use $ Global.scene . Scene.position

getScreenCenter :: Command State ScreenPosition
getScreenCenter = getScreenCenterFromSize <$> getScreenSize

getScreenCenterFromSize :: Size -> ScreenPosition
getScreenCenterFromSize = Position . flip scalarProduct 0.5 . view vector

getScreenLeftCenter :: Command State ScreenPosition
getScreenLeftCenter = getScreenLeftCenterFromSize <$> getScreenSize

getScreenLeftCenterFromSize :: Size -> ScreenPosition
getScreenLeftCenterFromSize s = Position (Vector2 0 (s ^. y / 2))

getScreenRightCenter :: Command State ScreenPosition
getScreenRightCenter = getScreenRightCenterFromSize <$> getScreenSize

getScreenRightCenterFromSize :: Size -> ScreenPosition
getScreenRightCenterFromSize s = Position (Vector2 (s ^. x) (s ^. y / 2))

getScreenSize :: Command State Size
getScreenSize = use $ Global.scene . Scene.size

translateToWorkspace :: ScreenPosition -> Command State Position
translateToWorkspace pos = do
    transformMatrix <- view (NodeEditor.screenTransform . screenToLogical) <$> Global.getNodeEditor
    let posMatrix      = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace = multStd2 posMatrix transformMatrix
    return $ Position $ fromTuple (getElem 1 1 posInWorkspace, getElem 1 2 posInWorkspace)
