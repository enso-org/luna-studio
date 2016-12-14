module Event.Window where

import           Data.Aeson        (ToJSON)
import           Luna.Studio.Prelude


data Event = Resized { _innerWidth  :: Int
                     , _innerHeight :: Int
                     } deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON Event
