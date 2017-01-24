module Luna.Studio.Event.Debug where


import           Data.Aeson          (ToJSON)
import           Luna.Studio.Prelude


data Event = GetState deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON Event
