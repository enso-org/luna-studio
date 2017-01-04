module Luna.Studio.State.Camera where

import           Data.Aeson                            (ToJSON)
import           Data.Matrix                           (getElem, multStd2)
import qualified Data.Matrix                           as Matrix
import           Luna.Studio.Commands.Command          (Command)
import           Luna.Studio.Data.CameraTransformation (screenToLogical)
import           Luna.Studio.Data.Vector               (Position (Position), ScreenPosition, Vector2 (Vector2), x, y)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import qualified Luna.Studio.React.Store               as Store

data State = PanDrag  { _panPreviousPos  :: ScreenPosition }
           | ZoomDrag { _zoomFixedPoint  :: Position
                      , _zoomPreviousPos :: ScreenPosition }
           deriving (Eq, Show, Generic)

makeLenses ''State
instance ToJSON State
