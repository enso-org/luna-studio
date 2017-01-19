module Luna.Studio.State.PenConnect where

import           Data.Aeson           (ToJSON)
import           Data.Position        (Position)
import           Empire.API.Data.Node (NodeId)
import           Luna.Studio.Prelude



data State = State { _history         :: [Position]
                   , _lastVisitedNode :: Maybe NodeId
                   } deriving (Eq, Generic, Show)

makeLenses ''State
instance ToJSON State
