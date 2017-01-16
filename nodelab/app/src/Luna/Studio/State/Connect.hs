module Luna.Studio.State.Connect where

import           Data.Aeson          (ToJSON)
import           Luna.Studio.Prelude

data State = State deriving (Eq, Generic, Show)

instance ToJSON State
makeLenses ''State
