module Luna.Studio.State.MultiSelection where


import           Data.Aeson              (ToJSON)
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude

data DragHistory = DragHistory { _dragStartPos   :: Position
                               , _dragCurrentPos :: Position
                               } deriving (Eq, Show, Generic)


data State = State { _history :: Maybe DragHistory
                   } deriving (Eq, Show, Generic)


makeLenses ''State
makeLenses ''DragHistory

instance ToJSON State
instance ToJSON DragHistory

instance Default State where
    def = State def
