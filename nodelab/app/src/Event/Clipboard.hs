module Event.Clipboard where

import           Data.Aeson        (ToJSON)
import           Luna.Studio.Prelude


data Event = Copy
           | Cut
           | Paste Text
             deriving (Eq, Show, Generic)

makeLenses ''Event

instance ToJSON Event
