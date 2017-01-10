module Luna.Studio.State.Camera where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (Position, ScreenPosition)
import           Luna.Studio.Prelude


data State = PanDrag  { _panPreviousPos  :: ScreenPosition }
           | ZoomDrag { _zoomFixedPoint  :: Position
                      , _zoomPreviousPos :: ScreenPosition }
           deriving (Eq, Show, Generic)

makeLenses ''State
instance ToJSON State
