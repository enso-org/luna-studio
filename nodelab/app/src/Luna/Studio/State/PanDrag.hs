module Luna.Studio.State.PanDrag where

import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector (ScreenPosition)
import           Luna.Studio.Prelude


data State = State { _previousPos  :: ScreenPosition
                   } deriving (Eq, Show, Generic)

makeLenses ''State
instance ToJSON State
