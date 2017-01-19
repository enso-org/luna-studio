module Luna.Studio.State.MultiSelection where

import           Data.Aeson          (ToJSON)
import           Data.Position       (Position)
import           Luna.Studio.Prelude


data State = State { _dragStartPos   :: Position
                   , _dragCurrentPos :: Position
                   } deriving (Eq, Show, Generic)

makeLenses ''State
instance ToJSON State
