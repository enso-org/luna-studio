module Luna.Studio.State.ZoomDrag where

import           Data.Aeson          (ToJSON)
import           Data.Position       (Position, ScreenPosition)
import           Luna.Studio.Prelude


data State = State { _fixedPoint  :: Position
                   , _previousPos :: ScreenPosition
                   } deriving (Eq, Show, Generic)

makeLenses ''State
instance ToJSON State
