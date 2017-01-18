module Luna.Studio.State.PenDisconnect where

import           Data.Aeson              (ToJSON)
import           Empire.API.Data.Node    (NodeId)
import           Luna.Studio.Data.Vector (Position)
import           Luna.Studio.Prelude



data State = State { _history         :: [Position]
                   , _lastVisitedNode :: Maybe NodeId
                   } deriving (Eq, Generic, Show)

makeLenses ''State
instance ToJSON State
