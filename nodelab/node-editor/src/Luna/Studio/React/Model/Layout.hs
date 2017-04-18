{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.React.Model.Layout where

import qualified Data.Matrix                           as Matrix
import           Data.Position                         (Position, move, vector)
import qualified Data.Position                         as Position
import           Data.ScreenPosition                   (ScreenPosition (ScreenPosition))
import qualified Data.ScreenPosition                   as ScreenPosition
import           Data.Size                             (Size)
import           Data.Vector2                           (Vector2 (Vector2), x, y)
import           Luna.Studio.Data.CameraTransformation (CameraTransformation, logicalToScreen, screenToLogical)
import           Luna.Prelude
import           Luna.Studio.React.Model.Constants     (gridSize)
import           Luna.Studio.React.Model.Port          (InPort, OutPort, getPositionInSidebar, portId)
import           Luna.Studio.React.Model.Sidebar       (InputSidebar, OutputSidebar, inputSidebarPosition, inputSidebarSize,
                                                        outputSidebarPosition, portPositionInInputSidebar, portPositionInOutputSidebar)

data Layout = Layout
        { _screenTransform :: CameraTransformation
        , _scene           :: Maybe Scene
        } deriving (Default, Eq, Generic, Show)

data Scene = Scene
        { _position      :: ScreenPosition
        , _size          :: Size
        , _inputSidebar  :: Maybe InputSidebar
        , _outputSidebar :: Maybe OutputSidebar
        } deriving (Default, Eq, Generic, Show)

makeLenses ''Layout
makeLenses ''Scene


translateToWorkspace :: ScreenPosition -> CameraTransformation -> Position
translateToWorkspace pos screenTransform'  =
    let transformMatrix = screenTransform' ^. screenToLogical
        posMatrix      = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace = Matrix.multStd2 posMatrix transformMatrix
    in Position.fromDoubles (Matrix.getElem 1 1 posInWorkspace) (Matrix.getElem 1 2 posInWorkspace)

translateToScreen :: Position -> CameraTransformation -> ScreenPosition
translateToScreen pos screenTransform' =
    let transformMatrix = screenTransform' ^. logicalToScreen
        posMatrix      = Matrix.fromList 1 4 [ pos ^. x, pos ^. y, 1, 1]
        posInWorkspace = Matrix.multStd2 posMatrix transformMatrix
    in ScreenPosition.fromDoubles (Matrix.getElem 1 1 posInWorkspace) (Matrix.getElem 1 2 posInWorkspace)

inputSidebarPortPosition :: OutPort -> Layout -> Maybe Position
inputSidebarPortPosition p layout = case layout ^? scene . traverse . inputSidebar . traverse of
    Just inputSidebar' ->
        let pid = p ^. portId
            shift = inputSidebar' ^. inputSidebarPosition . vector + Vector2 0 gridSize
            siz   = inputSidebar' ^. inputSidebarSize
            pos   = ScreenPosition . view ScreenPosition.vector . move shift $
                maybe (portPositionInInputSidebar siz pid) id (getPositionInSidebar p)
        in Just $ translateToWorkspace pos $ layout ^. screenTransform
    _ -> Nothing

outputSidebarPortPosition :: InPort -> Layout -> Maybe Position
outputSidebarPortPosition p layout = case layout ^? scene . traverse . outputSidebar . traverse of
    Just outputSidebar' ->
        let pid = p ^. portId
            shift = outputSidebar' ^. outputSidebarPosition . vector + Vector2 0 gridSize
            pos   = ScreenPosition . view ScreenPosition.vector . move shift $
                maybe (portPositionInOutputSidebar pid) id (getPositionInSidebar p)
        in Just $ translateToWorkspace pos $ layout ^. screenTransform
    _ -> Nothing
